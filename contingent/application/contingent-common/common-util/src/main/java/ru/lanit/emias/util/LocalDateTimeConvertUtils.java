package ru.lanit.emias.util;

import java.time.DayOfWeek;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.time.temporal.TemporalAccessor;
import java.time.temporal.TemporalQueries;
import java.util.List;

/**
 * @author mkomlev
 */
public final class LocalDateTimeConvertUtils {

    private final static DateTimeFormatter DATE_TIME_FORMAT;

    static {
        DateTimeFormatterBuilder builder = new DateTimeFormatterBuilder()
                .parseCaseInsensitive()
                .append(DateTimeFormatter.ISO_LOCAL_DATE)
                .appendOptional(new DateTimeFormatterBuilder()
                        .parseCaseInsensitive()
                        .appendLiteral('T')
                        .append(DateTimeFormatter.ISO_LOCAL_TIME)
                        .appendOptional(new DateTimeFormatterBuilder()
                        		.appendOffsetId()
                        		.toFormatter())
                        .toFormatter());
        DATE_TIME_FORMAT = builder.toFormatter();
    }

    private LocalDateTimeConvertUtils() { }

    public static String print(LocalDateTime localDateTime) {
        return localDateTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
    }

    public static LocalDateTime parse(String src) {
        TemporalAccessor accessor = DATE_TIME_FORMAT.parse(src);
        LocalDate date = LocalDate.from(accessor);
        LocalTime time = accessor.query(TemporalQueries.localTime());
        // Временную зону игнорируем, т.к. в общем случае ее невозможно использовать
        //ZoneOffset offset = accessor.query(TemporalQueries.offset());
        if (time == null) {
            return LocalDateTime.of(date, LocalTime.MIN);
        }
        return LocalDateTime.of(date, time);
    }

    public static LocalDateTime convertDateToDayStart (LocalDate date) {
        if (date == null) {
            return null;
        }
        return date.atStartOfDay();
    }

    public static LocalDateTime convertDateToNextDayStart (LocalDate date) {
        if (date == null) {
            return null;
        }
        return date.plusDays(1L).atStartOfDay();
    }

    public static LocalDateTime currentZoneLocalDateTime(String timeZone) {
        Instant now = Instant.now();
        ZoneId zoneId = ZoneId.of(timeZone);
        ZonedDateTime zdt = ZonedDateTime.ofInstant(now, zoneId);

        return zdt.toLocalDateTime();
    }

    public static LocalDateTime addDaysExceptHolidays(LocalDateTime startDate, long daysToAdd, List<LocalDate> holidays) {
        if (daysToAdd > 0) {
            LocalDateTime targetDate = startDate;
            do {
                targetDate = targetDate.plusDays(1);
                if (!isWeekend(targetDate) && !holidays.contains(targetDate.toLocalDate())) {
                    daysToAdd--;
                    if (daysToAdd == 0) {
                        break;
                    }
                }
            } while (daysToAdd > 0);
            return targetDate;
        }
        return startDate;
    }

    public static LocalDateTime minusDaysExceptHolidays(LocalDateTime startDate, long daysToMinus, List<LocalDate> holidays) {
        if (daysToMinus > 0) {
            LocalDateTime targetDate = startDate;
            do {
                targetDate = targetDate.minusDays(1);
                if (!isWeekend(targetDate) && !holidays.contains(targetDate.toLocalDate())) {
                    daysToMinus--;
                    if (daysToMinus == 0) {
                        break;
                    }
                }
            } while (daysToMinus > 0);
            return targetDate;
        }
        return startDate;
    }

    private static boolean isWeekend(LocalDateTime day) {
        DayOfWeek dayOfWeek = day.getDayOfWeek();
        return dayOfWeek == DayOfWeek.SATURDAY || dayOfWeek == DayOfWeek.SUNDAY;
    }

}