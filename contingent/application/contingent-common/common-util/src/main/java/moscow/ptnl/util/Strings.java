package moscow.ptnl.util;

import org.springframework.util.StringUtils;

import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 *
 * @author m.kachalov
 */
public class Strings {

    private static final Pattern NUMBER_WITH_4_DIGITS = Pattern.compile("^\\d{1,4}$");

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

    public static boolean isNumberWith4Digits(String source) {
        Matcher m = NUMBER_WITH_4_DIGITS.matcher(source);
        return m.matches();
    }
}
