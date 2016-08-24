package se.kth.edx.id2203.validation

import java.nio.file.{Files, Paths}
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.security.{KeyFactory, PrivateKey, PublicKey}
import javax.crypto.Cipher


object Encryptor {

  private val defkey = readPubKey("rsa/edx-key-pub.der");

  def encrypt(publicKey: PublicKey, plainTxt: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA1AndMGF1Padding");
    cipher.init(Cipher.ENCRYPT_MODE, publicKey);
    cipher.doFinal(plainTxt);
  }

  def encrypt(plainTxt: Array[Byte]): Array[Byte] = {
    encrypt(defkey, plainTxt)
  }

  def encrypt(plainTxt: String): Array[Byte] = {
    encrypt(defkey, plainTxt.toCharArray.map(_.toByte))
  }

  def decrypt(privateKey: PrivateKey, encryptedRaw: String) : Array[Byte] = {
    decrypt(privateKey, encryptedRaw.toCharArray.map(_.toByte))
  }
  
  def decrypt(privateKey: PrivateKey, encryptedRaw: Array[Byte]) : Array[Byte] = {
    val cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA1AndMGF1Padding");
    cipher.init(Cipher.DECRYPT_MODE, privateKey);
    cipher.doFinal(encryptedRaw);
  }

  def readPrivateKey(path: String) : PrivateKey = KeyFactory.getInstance("RSA").generatePrivate(new PKCS8EncodedKeySpec(readFileBytes(path)))
  
  def readPubKey(path: String): PublicKey = {
    val pubKeyBytes = readFileBytes(path);
    KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(pubKeyBytes))
  }

  def readFileBytes(path: String): Array[Byte] = Files.readAllBytes(Paths.get(path))

}

