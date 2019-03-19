package ru.lanit.emias.util;

import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.ChronoField;

/**
 * @author mkomlev
 */
public final class LocalTimeConvertUtils {
    private LocalTimeConvertUtils() { }

    private static final DateTimeFormatter TIME_FORMAT = new DateTimeFormatterBuilder()
        .appendValue(ChronoField.HOUR_OF_DAY, 2)
        .appendLiteral(':')
        .appendValue(ChronoField.MINUTE_OF_HOUR, 2)
        .appendLiteral(':')
        .appendValue(ChronoField.SECOND_OF_MINUTE, 2)
        .appendOptional(new DateTimeFormatterBuilder()
        		.appendOffsetId()
        		.toFormatter())
        .toFormatter();

    public static String print(LocalTime localTime) {
        return localTime.format(TIME_FORMAT);
    }

    public static LocalTime parse(String src) {
    	// Временную зону игнорируем, т.к. в общем случае ее невозможно использовать
        return LocalTime.parse(src, TIME_FORMAT);
    }
}
