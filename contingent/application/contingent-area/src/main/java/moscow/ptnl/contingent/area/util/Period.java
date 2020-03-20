package moscow.ptnl.contingent.area.util;

import java.time.LocalDate;
import java.util.Objects;

public class Period {

    public static final LocalDate MIN_DATE = LocalDate.of(1900, 1, 1);
    public static final LocalDate MAX_DATE = LocalDate.of(2999, 1, 1);
    public static final Period ALL_TIME = new Period(MIN_DATE, MAX_DATE);

    private LocalDate startDate;
    private LocalDate endDate;

    public LocalDate getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDate startDate) {
        this.startDate = startDate;
    }

    public LocalDate getEndDate() {
        return endDate;
    }

    public void setEndDate(LocalDate endDate) {
        this.endDate = endDate;
    }

    public Period(LocalDate startDate, LocalDate endDate) {
        this.startDate = startDate;
        this.endDate = endDate;
    }

    public boolean isInterceptWith(Period other) {
        return isInterceptWith(other.getStartDate(), other.getEndDate());
    }

    public boolean isInterceptWith(LocalDate otherStartDate, LocalDate otherEndDate) {
        if (otherStartDate == null && (otherEndDate == null || this.startDate.isBefore(otherEndDate.plusDays(1)))
                || this.endDate == null && otherEndDate == null
                || otherStartDate != null && otherStartDate.isBefore(this.startDate.plusDays(1)) && otherEndDate != null
                    && this.startDate.isBefore(otherEndDate.plusDays(1))
                || otherStartDate !=null && this.startDate.isBefore(otherStartDate.plusDays(1)) && this.endDate != null
                    && otherStartDate.isBefore(this.endDate.plusDays(1))) {
            return true;
        }
        return false;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Period)) return false;

        Period period = (Period) o;

        if (!Objects.equals(startDate, period.startDate)) return false;
        return Objects.equals(endDate, period.endDate);
    }

    @Override
    public int hashCode() {
        int result = startDate != null ? startDate.hashCode() : 0;
        result = 31 * result + (endDate != null ? endDate.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        return "Period{" +
                "startDate=" + startDate +
                ", endDate=" + endDate +
                '}';
    }
}
