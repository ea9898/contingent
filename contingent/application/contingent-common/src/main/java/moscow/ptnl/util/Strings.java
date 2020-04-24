package moscow.ptnl.util;

import org.springframework.util.StringUtils;

import java.util.Arrays;
import java.util.stream.Collectors;

/**
 *
 * @author m.kachalov
 */
public class Strings {
    
    private Strings(){}

    public static String toCamelCase(String value) {
        String[] words = value.split("_");
        return StringUtils.uncapitalize(Arrays.stream(words)
                .map(String::toLowerCase)
                .map(StringUtils::capitalize)
                .collect(Collectors.joining("")));
    }

    public static boolean isNullOrEmpty(String source) {
        return source == null || source.isEmpty();
    }
    
}
