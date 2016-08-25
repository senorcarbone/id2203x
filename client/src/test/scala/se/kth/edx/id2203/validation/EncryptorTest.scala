package se.kth.edx.id2203.validation

import org.scalatest._
import java.nio.charset.StandardCharsets.UTF_8

class EncyptorTest extends FlatSpec with Matchers {

    val smallData = "Test";
    val biggerData = {
        val sb = new StringBuilder();
        for (i <- 1 to 1000) {
            sb.append(i);
        }
        sb.toString()
    };
    //val pubkey = Encryptor.readPubKey("test.key");
    //val privkey = Encryptor.readPrivateKey("test.key");

    "Encryptor" should "find test key" in {        
        Encryptor.checkPublicKey() should be (true);        
        Encryptor.checkPrivateKey() should be (true);
    }
    
    it should "transcrypt small data (< block size)" in {
        val enc = Encryptor.encrypt(smallData);
        enc should not be (null);
        enc should not be empty;
        println(Encryptor.formatBytes(enc))
        val dec = Encryptor.decryptText(enc);
        dec should not be (null);
        dec should equal (smallData);
    }
    
    it should "transcrypt bigger data (> block size)" in {
        val enc = Encryptor.encrypt(biggerData);
        enc should not be (null);
        enc should not be empty;
        println(Encryptor.formatBytes(enc))
        val dec = Encryptor.decryptText(enc);
        dec should not be (null);
        dec should equal (biggerData);
    }
}