package moscow.ptnl.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.invoke.MethodHandles;
import java.net.InetAddress;

public class CommonUtils {

    private static final Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    public static String getHostName() {
        String h = "localhost";

        try {
            InetAddress address = InetAddress.getLocalHost();
            h = address.getHostName() == null ? address.getHostAddress() : address.getHostName();
        } catch(Exception ex) {
            LOG.warn("Could not determine host name");
        }
        return h;
    }
}
