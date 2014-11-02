package crypto.week2

import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

/**
 * Created by mike on 02.11.14.
 */
object Week2 {


  def main(args: Array[String]): Unit = {

    val cbcKeyData = BigInt("140b41b22a29beb4061bda66b6747e14", 16).toByteArray
    val cbc1CiphData = BigInt("4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee2e4b7465d5290d0c0e6c6822236e1daafb94ffe0c5da05d9476be028ad7c1d81", 16).toByteArray
    decCbc(cbcKeyData, cbc1CiphData)

    val cbc2CiphData = BigInt("5b68629feb8606f9a6667670b75b38a5b4832d0f26e1ab7da33249de7d4afc48e713ac646ace36e872ad5fb8a512428a6e21364b0c374df45503473c5242a253", 16).toByteArray
    decCbc(cbcKeyData, cbc2CiphData)

    val ctrKeyData = BigInt("36f18357be4dbd77f050515c73fcf9f2", 16).toByteArray
    val ctr1CiphData = BigInt("69dda8455c7dd4254bf353b773304eec0ec7702330098ce7f7520d1cbbb20fc388d1b0adb5054dbd7370849dbf0b88d393f252e764f1f5f7ad97ef79d59ce29f5f51eeca32eabedd9afa9329", 16).toByteArray
    decCtr(ctrKeyData, ctr1CiphData)

    val ctr2CiphData = BigInt("770b80259ec33beb2561358a9f2dc617e46218c0a53cbeca695ae45faa8952aa0e311bde9d4e01726d3184c34451", 16).toByteArray
    decCtr(ctrKeyData, ctr2CiphData)

  }


  def decCbc(keyData: Array[Byte], ciphData: Array[Byte]): Unit = {

    val result = decAesCbc(keyData, ciphData)
    val resStr = hexDataToString(result)
    println(s"Cbc: $resStr")

    // check with builtin CBC decryption
//    val cipher2 = Cipher.getInstance("AES/CBC/PKCS5Padding")
//    val key = new SecretKeySpec(keyData, "AES")
//    val IV = new IvParameterSpec(ciphData.take(16))
//    cipher2.init(Cipher.DECRYPT_MODE, key, IV)
//    val res2 = cipher2.doFinal(ciphData.drop(16))
//    val result2 = hexDataToString(res2)
//    println(s"Cbc2: $result2")
  }

  def decCtr(keyData: Array[Byte], ciphData: Array[Byte]): Unit = {
    val result = decAesCtr(keyData, ciphData)
    val resStr = hexDataToString(result)
    println(s"Ctr: $resStr")
  }

  def decAesCbc(keyData: Array[Byte], ciphData: Array[Byte]) = {
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    val key = new SecretKeySpec(keyData, "AES")
    cipher.init(Cipher.DECRYPT_MODE, key)

    val iv = ciphData.take(16)
    val ciphBlocks = ciphData.drop(16).sliding(16, 16)

    var prevBlock = iv

    val decData =
      for (block <- ciphBlocks) yield {
        val decBlock = cipher.doFinal(block)
        val cbcBlock = decBlock.zip(prevBlock).map(b => (b._1 ^ b._2).toByte)
        prevBlock = block
        cbcBlock
      }

    // todo: handle pkcs5 padding

    decData.flatten.toArray
  }

  def decAesCtr(keyData: Array[Byte], ciphData: Array[Byte]) = {
    val IV = BigInt(ciphData.take(16))

    val ciphBlocks = ciphData.drop(16).sliding(16, 16).toList

    def decCtrBlock(counter: Int, block: Array[Byte]) = {
      val cipher = Cipher.getInstance("AES/ECB/NoPadding")
      val ivCtr = IV + counter
      val ivBlock = ivCtr.toByteArray
      val key = new SecretKeySpec(keyData, "AES")
      cipher.init(Cipher.ENCRYPT_MODE, key)
      val ctrEnc = cipher.doFinal(ivBlock)
      block.zip(ctrEnc).map(b => (b._1 ^ b._2).toByte)
    }

    // parallelizable - try it just for fun :)
    val result = ciphBlocks.zipWithIndex.par.map(x => decCtrBlock(x._2, x._1))
    result.flatten.seq.toArray
  }

  def hexDataToString(data: Array[Byte]) = {
    new String(data.map(_.toChar))
  }

}
