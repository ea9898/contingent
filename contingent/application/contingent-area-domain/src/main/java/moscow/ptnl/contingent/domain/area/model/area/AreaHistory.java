package moscow.ptnl.contingent.domain.area.model.area;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;

import javax.persistence.Column;
import javax.persistence.ColumnResult;
import javax.persistence.ConstructorResult;
import javax.persistence.Entity;
import javax.persistence.EntityResult;
import javax.persistence.FieldResult;
import javax.persistence.Id;
import javax.persistence.SqlResultSetMapping;
import java.io.Serializable;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

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
