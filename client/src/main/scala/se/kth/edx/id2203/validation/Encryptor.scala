package se.kth.edx.id2203.validation

import java.nio.file.{ Files, Paths, LinkOption }
import java.io.{ InputStream, ByteArrayInputStream, ByteArrayOutputStream }
import java.security.spec.{ PKCS8EncodedKeySpec, X509EncodedKeySpec }
import java.security.{ KeyFactory, PrivateKey, PublicKey, Security }
import javax.crypto.Cipher
import com.typesafe.config.{ Config, ConfigFactory }
import com.typesafe.scalalogging.StrictLogging
import java.nio.charset.StandardCharsets.UTF_8
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.bcpg.SymmetricKeyAlgorithmTags
import org.bouncycastle.openpgp._
import org.bouncycastle.openpgp.operator.jcajce._
import org.bouncycastle.openpgp.operator.KeyFingerPrintCalculator;
import org.bouncycastle.bcpg.ArmoredOutputStream;
import org.bouncycastle.openpgp.jcajce.JcaPGPObjectFactory;
import resource._
import org.bouncycastle.util.io.Streams;
import scala.collection.JavaConverters._

object Encryptor extends StrictLogging {

    Security.addProvider(new BouncyCastleProvider());

    //private lazy val defkey = readPubKey("rsa/edx-key-pub.der");
    private val config = ConfigFactory.load();
    private val pubKey = readPubKey(config.getString("kthx.client.crypto.publicKey"));
    private val privateKey = readPrivateKey(config.getString("kthx.client.crypto.privateKey"));

    def formatBytes(bytes: Array[Byte]): String = bytes.map("%02X" format _).mkString

    private def encrypt(publicKey: PGPPublicKey, plainTxt: Array[Byte]): Array[Byte] = {
        val encGen = new PGPEncryptedDataGenerator(
            new JcePGPDataEncryptorBuilder(SymmetricKeyAlgorithmTags.AES_128).
                setWithIntegrityPacket(true).
                setSecureRandom(new java.security.SecureRandom()).
                setProvider("BC"));
        encGen.addMethod(new JcePublicKeyKeyEncryptionMethodGenerator(publicKey).setProvider("BC"));

        val tbout = new ByteArrayOutputStream();
        val ldGen = new PGPLiteralDataGenerator();
        for (ldStream <- managed(ldGen.open(tbout, PGPLiteralData.BINARY, "EncData", plainTxt.length, new java.util.Date()))) {
            ldStream.write(plainTxt);
        }
        tbout.close();
        val plainObject = tbout.toByteArray();
        logger.info(s"Encoded ${plainObject}")

        val bout = new ByteArrayOutputStream();
        for (
            aout <- managed(new ArmoredOutputStream(bout));
            cout <- managed(encGen.open(aout, plainObject.length))
        ) {
            cout.write(plainObject);
        }
        bout.toByteArray()
    }

    private def decrypt(privateKey: PGPSecretKey, encryptedRaw: Array[Byte]): Array[Byte] = {
        var res: Array[Byte] = null;
        for (
            bin <- managed(new ByteArrayInputStream(encryptedRaw));
            din <- managed(PGPUtil.getDecoderStream(bin))
        ) {
            val pgpF = new JcaPGPObjectFactory(din);
            val enc = pgpF.nextObject() match {
                case x: PGPEncryptedDataList => x
                case _                       => pgpF.nextObject().asInstanceOf[PGPEncryptedDataList]
            }

            val it = enc.getEncryptedDataObjects();
            val pbe = it.next().asInstanceOf[PGPPublicKeyEncryptedData];

            val secretKey = privateKey.extractPrivateKey(
                new JcePBESecretKeyDecryptorBuilder().
                    setProvider("BC").
                    build(null));
            val decFac = new JcePublicKeyDataDecryptorFactoryBuilder().
                setProvider("BC").
                build(secretKey);
            for (
                clear <- managed(pbe.getDataStream(decFac))
            ) {
                //logger.info(scala.io.Source.fromInputStream(clear).mkString);
                val plainFact = new JcaPGPObjectFactory(clear);

                val m2 = plainFact.nextObject() match {
                    case x: PGPCompressedData => {
                        val pgpFact = new JcaPGPObjectFactory(x.getDataStream());
                        pgpFact.nextObject();
                    }
                    case x => x
                }

                val m3 = m2 match {
                    case x: PGPLiteralData => {

                        val unc = x.getInputStream();
                        val bout = new ByteArrayOutputStream()
                        Streams.pipeAll(unc, bout);

                        val bytes = bout.toByteArray();
                        bout.close();
                        bytes
                    }
                    case _: PGPOnePassSignatureList =>
                        throw new PGPException("encrypted message contains a signed message - not literal data.");
                    case _ =>
                        throw new PGPException("message is not a simple encrypted file - type unknown.");
                }

                res = m3

                if (pbe.isIntegrityProtected()) {
                    if (!pbe.verify()) {
                        logger.warn("message failed integrity check");
                    } else {
                        logger.info("message integrity check passed");
                    }
                } else {
                    logger.debug("no message integrity check");
                }
            }
        }
        res
    }

    def encrypt(plainTxt: Array[Byte]): Array[Byte] = {
        encrypt(pubKey.get, plainTxt)
    }

    def encrypt(plainTxt: String): Array[Byte] = {
        encrypt(pubKey.get, plainTxt.getBytes(UTF_8))
    }

    def decryptText(encryptedRaw: Array[Byte]): String = {
        val decryptedBytes = decrypt(encryptedRaw);
        new String(decryptedBytes, UTF_8)
    }

    def decrypt(encryptedRaw: Array[Byte]): Array[Byte] = {
        decrypt(privateKey.get, encryptedRaw)
    }

    //    private def decrypt(privateKey: PGPSecretKey, encryptedRaw: String): Array[Byte] = {
    //        decrypt(privateKey, encryptedRaw.getBytes(UTF_8))
    //    }

    def checkPrivateKey(): Boolean = privateKey.isDefined;
    def checkPublicKey(): Boolean = pubKey.isDefined;

    def readPrivateKey(path: String): Option[PGPSecretKey] = {
        readFileBytes(path).orElse(readResourceBytes(path)) match {
            case Some(keyStream) => {
                val pgpSecret = pgpPrivateKeyRings(keyStream, new JcaKeyFingerprintCalculator());

                //
                // we just loop through the collection till we find a key suitable for encryption, in the real
                // world you would probably want to be a bit smarter about this.
                //

                val keyRingIter = pgpSecret.getKeyRings().asInstanceOf[java.util.Iterator[PGPSecretKeyRing]];
                var key: Option[PGPSecretKey] = None;
                while (key.isEmpty && keyRingIter.hasNext) {
                    val kRing = keyRingIter.next();
                    val kIt = kRing.getSecretKeys();
                    while (key.isEmpty && kIt.hasNext) {
                        val k = kIt.next();
                        if (k.isSigningKey()) {
                            key = Some(k);
                        } else {
                            logger.warn(s"This key is not a signing key: $k")
                        }
                    }
                }
                keyStream.close();
                key
            }
            case _ => logger.warn(s"Could not read public key from file $path"); None
        }
    }

    def readPubKey(path: String): Option[PGPPublicKey] = {
        readFileBytes(path).orElse(readResourceBytes(path)) match {
            case Some(keyStream) => {
                logger.info(s"There are ${keyStream.available()} bytes available in the key stream.")
                val pgpPub = pgpPublicKeyRings(keyStream, new JcaKeyFingerprintCalculator());

                //
                // we just loop through the collection till we find a key suitable for encryption, in the real
                // world you would probably want to be a bit smarter about this.
                //

                val keyRingIter = pgpPub.getKeyRings().asInstanceOf[java.util.Iterator[PGPPublicKeyRing]];
                var key: Option[PGPPublicKey] = None;
                while (key.isEmpty && keyRingIter.hasNext) {
                    val kRing = keyRingIter.next();
                    val kIt = kRing.getPublicKeys();
                    while (key.isEmpty && kIt.hasNext) {
                        val k = kIt.next();
                        if (k.isEncryptionKey) {
                            key = Some(k);
                        }
                    }
                }
                keyStream.close();
                key
            }
            case _ => logger.warn(s"Could not read public key from file $path"); None
        }
    }

    private def readFileBytes(pathS: String): Option[InputStream] = {
        val path = Paths.get(pathS).toAbsolutePath();
        if (Files.exists(path)) {
            if (Files.isReadable(path)) {
                val bytes = Files.readAllBytes(path)
                Some(new ByteArrayInputStream(bytes))
            } else {
                logger.warn(s"Not allowed to read key file at ${path}.")
                None
            }
        } else {
            logger.warn(s"Could not find key file at ${path}.")
            None
        }
    }

    private def readResourceBytes(pathS: String): Option[InputStream] = {
        val is = Thread.currentThread().getContextClassLoader().getResourceAsStream(pathS);
        if (is != null) {
            Some(is)
        } else {
            logger.warn(s"Could not find key resource at ${pathS}.")
            None
        }
    }

    private def pgpPrivateKeyRings(in: InputStream, fingerPrintCalculator: KeyFingerPrintCalculator): PGPSecretKeyRingCollection = {
        val keyRings = pgpKeyRings(in, fingerPrintCalculator);
        val secretRings = keyRings collect {
            case x: PGPSecretKeyRing => x
        }
        new PGPSecretKeyRingCollection(secretRings.asJava)
    }

    private def pgpPublicKeyRings(in: InputStream, fingerPrintCalculator: KeyFingerPrintCalculator): PGPPublicKeyRingCollection = {
        val keyRings = pgpKeyRings(in, fingerPrintCalculator);
        val publicRings = keyRings collect {
            case x: PGPPublicKeyRing => x
        }
        new PGPPublicKeyRingCollection(publicRings.asJava)
    }

    private def pgpKeyRings(in: InputStream, fingerPrintCalculator: KeyFingerPrintCalculator): Seq[PGPKeyRing] = {
        logger.info(s"There are ${in.available()} bytes available to read key rings from.")
        val col = scala.collection.Seq.newBuilder[PGPKeyRing];

        while (in.available() > 0) {
            logger.info(s"There are ${in.available()} bytes remaining.")
            val din = PGPUtil.getDecoderStream(in);
            val pgpFact = new PGPObjectFactory(din, fingerPrintCalculator);
            var obj: Any = pgpFact.nextObject();

            while (obj != null) {
                obj match {
                    case x: PGPKeyRing =>
                        col += x; logger.info(s"Added key ring object to collection: $x")
                    case x => logger.warn(s"Object from InputStream was not a KeyRing but ${x.getClass()}")
                }
                obj = pgpFact.nextObject();
            }
        }
        col.result()
        //        val pgpIt = pgpFact.asScala;
        //        val r = pgpIt flatMap {
        //            case x: PGPKeyRing =>
        //                logger.info(s"Added key ring object to collection: $x"); Some(x)
        //            case x => logger.warn(s"Object from InputStream was not a KeyRing but ${x.getClass()}"); None
        //        }
        //        r.toSeq
    }

}

