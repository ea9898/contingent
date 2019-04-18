package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.converter.BooleanIntegerConverter;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.Date;

@Entity
@Table(name = "ADDRESS_ALLOCATION_ORDER")
@Cacheable
public class AddressAllocationOrder implements Serializable {

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

    @Size(max = 50)
    @Column(name = "OUZ")
    private String ouz;

    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean archived;

    @Column(name = "CREATE_DATE", nullable = false)
    private Date createDate;

    @Column(name = "UPDATE_DATE", nullable = false)
    private Date updateDate;

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

    public String getOuz() {
        return ouz;
    }

    public void setOuz(String ouz) {
        this.ouz = ouz;
    }

    public Boolean getArchived() {
        return archived;
    }

    public void setArchived(Boolean archived) {
        this.archived = archived;
    }

    public Date getCreateDate() {
        return createDate;
    }

    public void setCreateDate(Date createDate) {
        this.createDate = createDate;
    }

    public Date getUpdateDate() {
        return updateDate;
    }

    public void setUpdateDate(Date updateDate) {
        this.updateDate = updateDate;
    }
}
