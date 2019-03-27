package ru.lanit.emias2.contingent.state.cache;

import org.springframework.util.SerializationUtils;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

/**
 * Created by mkomlev on 05.05.2016.
 *
 */
public class ObjectKey {
    private ObjectKey() {

    }

    public static String get(Object object) {
        if (object == null) {
            return null;
        }

        byte[] src = SerializationUtils.serialize(object);

        try {
            MessageDigest hash = MessageDigest.getInstance("MD5");
            byte[] bytes = hash.digest(src);
            StringBuilder hexString = new StringBuilder();
            for (byte aByte : bytes) {
                String hex = Integer.toHexString(0xFF & aByte);
                if (hex.length() == 1) {
                    hexString.append('0');
                }
                hexString.append(hex);
            }
            return hexString.toString();
        } catch (NoSuchAlgorithmException e) {
            return String.valueOf(Arrays.hashCode(src));
        }
    }


}
