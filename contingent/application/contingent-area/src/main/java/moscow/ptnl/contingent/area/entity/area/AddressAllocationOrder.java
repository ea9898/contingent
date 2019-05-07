package moscow.ptnl.contingent.area.entity.area;

import moscow.ptnl.contingent.area.entity.converter.BooleanIntegerConverter;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;

@Entity
@Table(name = "ADDRESS_ALLOCATION_ORDER")
@SequenceGenerator(name = "SEQ_ADDRESS_ALLOCATION_ORDER", sequenceName = "SEQ_ADDRESS_ALLOCATION_ORDER", allocationSize=1)
@Cacheable
public class AddressAllocationOrder implements Serializable {

    private static final long serialVersionUID = -8116611274341177299L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_ADDRESS_ALLOCATION_ORDER")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Size(max = 255)
    @Column(name = "NUMBER", nullable = false)
    private String number;

    @Size(max = 255)
    @Column(name = "NAME")
    private String name;

    @Column(name = "DATE")
    private LocalDate date;

    @Size(max = 50)
    @Column(name = "OUZ")
    private String ouz;

    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanIntegerConverter.class)
    private Boolean archived;

    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @Column(name = "UPDATE_DATE", nullable = false)
    private LocalDateTime updateDate;

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

    public LocalDate getDate() {
        return date;
    }

    public void setDate(LocalDate date) {
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

    public LocalDateTime getCreateDate() {
        return createDate;
    }

    public void setCreateDate(LocalDateTime createDate) {
        this.createDate = createDate;
    }

    public LocalDateTime getUpdateDate() {
        return updateDate;
    }

    public void setUpdateDate(LocalDateTime updateDate) {
        this.updateDate = updateDate;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof AddressAllocationOrder) {
            return ((AddressAllocationOrder) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }
}
