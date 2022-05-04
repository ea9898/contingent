package moscow.ptnl.contingent.domain.area.model.area;

import org.springframework.data.domain.Page;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDateTime;

public class AreaHistory {

    private long areaId;

    private LocalDateTime dateCreated;

    private Page<Event> events;

    public long getAreaId() { return areaId; }

    public void setAreaId(long areaId) { this.areaId = areaId;}

    public LocalDateTime getDateCreated() { return dateCreated; }

    public void setDateCreated(LocalDateTime dateCreated) { this.dateCreated = dateCreated; }

    public Page<Event> getEvents() { return events; }

    public void setEvents(Page<Event> events) { this.events = events; }

    public static interface Event {

        BigInteger getJobId();

        String getSnils();

        BigDecimal getIsReplacement();

        String getLogin();

        LocalDateTime getUpdateDate();

        String getStartDate();

        String getEndDate();

        String getIsError();

    }

}
