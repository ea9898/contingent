package moscow.ptnl.contingent.nsi.domain.area;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import org.hibernate.annotations.Proxy;

import java.io.Serializable;

@Entity
@Table(name = "EVENT_TYPES")
@Proxy(lazy=false)
@SequenceGenerator(name = "SEQ_EVENT_TYPES", sequenceName = "SEQ_EVENT_TYPES", allocationSize=1)
public class EventTypes implements Serializable {

    private static final long serialVersionUID = -900591131445072185L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_EVENT_TYPES")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Column(name = "NAME")
    private String name;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
