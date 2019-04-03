package moscow.ptnl.contingent.area.entity.area;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.Date;

@Entity
@Table(name = "AREA_MEDICAL_EMPLOYEE")
@Cacheable
public class OrderAddressAllocation implements Serializable {

    private static final long serialVersionUID = -8116611274341177299L;

    @Id
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

    @Size(max = 255)
    @Column(name = "NUMBER", nullable = false)
    private String number;

    @Size(max = 255)
    @Column(name = "NAME")
    private String name;

    @Column(name = "DATE")
    private Date date;

    @Column(name = "MO_ID")
    private Long moId;

    @Size(max = 50)
    @Column(name = "OUZ")
    private String ouz;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getNumber() {
        return number;
    }

    public void setNumber(String number) {
        this.number = number;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    public Long getMoId() {
        return moId;
    }

    public void setMoId(Long moId) {
        this.moId = moId;
    }

    public String getOuz() {
        return ouz;
    }

    public void setOuz(String ouz) {
        this.ouz = ouz;
    }
}
