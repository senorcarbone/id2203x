package se.kth.edx.id2203.validation

import java.nio.file.{Files, Paths}
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.security.{KeyFactory, PrivateKey, PublicKey}
import javax.crypto.Cipher


object Encryptor {

  private val defkey = readPubKey("rsa/edx.pub");

  def encrypt(publicKey: PublicKey, plainText: String): String = {
    new String(encrypt(publicKey, plainText.toCharArray.map(_.toByte)))
  }

  def encrypt(publicKey: PublicKey, plainTxt: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA1AndMGF1Padding");
    cipher.init(Cipher.ENCRYPT_MODE, publicKey);
    cipher.doFinal(plainTxt);
  }

  def encrypt(plainText: String): String = {
    encrypt(defkey, plainText)
  }

  def encrypt(plainTxt: Array[Byte]): Array[Byte] = {
    encrypt(defkey, plainTxt)
  }
  
  def decrypt(privateKey: PrivateKey, encryptedRaw: Array[Byte]) : Array[Byte] = {
    val cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA1AndMGF1Padding");
    cipher.init(Cipher.DECRYPT_MODE, privateKey);
    cipher.doFinal(encryptedRaw);
  }
  
  def decrypt(privateKey: PrivateKey, encryptedText: String) : String = {
    new String(decrypt(privateKey, encryptedText.toCharArray.map(_.toByte)))
  }

  def readPrivateKey(path: String) : PrivateKey = KeyFactory.getInstance("RSA").generatePrivate(new PKCS8EncodedKeySpec(readFileBytes(path)))
  
  def readPubKey(path: String): PublicKey = KeyFactory.getInstance("RSA").generatePublic(new X509EncodedKeySpec(readFileBytes(path)))

  def readFileBytes(path: String): Array[Byte] = Files.readAllBytes(Paths.get(path))

}

