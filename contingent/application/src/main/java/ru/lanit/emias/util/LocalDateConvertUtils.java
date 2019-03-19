package ru.lanit.emias.util;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.format.SignStyle;
import java.time.temporal.ChronoField;


/**
 * @author mkomlev
 */
public final class LocalDateConvertUtils {
    private LocalDateConvertUtils() { }

    private static final DateTimeFormatter DATE_FORMAT = new DateTimeFormatterBuilder()
            .appendValue(ChronoField.YEAR, 4, 10, SignStyle.EXCEEDS_PAD)
            .appendLiteral('-')
            .appendValue(ChronoField.MONTH_OF_YEAR, 2)
            .appendLiteral('-')
            .appendValue(ChronoField.DAY_OF_MONTH, 2)
            .appendOptional(new DateTimeFormatterBuilder()
            		.appendOffsetId()
            		.toFormatter())
        .toFormatter();

    public static String print(LocalDate localDate) {
        if (null == localDate)
            return null;
        return localDate.format(DATE_FORMAT);
    }

    public static String print(LocalDateTime localDate) {
        if (null == localDate)
            return null;
        return localDate.format(DATE_FORMAT);
    }

    public static LocalDate parse(String src) {
        if (src == null || src.isEmpty())
            return null;
        // Временную зону игнорируем, т.к. в общем случае ее невозможно использовать
        return LocalDate.parse(src, DATE_FORMAT);
    }
}
