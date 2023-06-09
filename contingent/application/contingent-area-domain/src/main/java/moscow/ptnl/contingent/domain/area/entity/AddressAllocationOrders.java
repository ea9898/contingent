package moscow.ptnl.contingent.domain.area.entity;

import moscow.ptnl.contingent.domain.converter.BooleanStrictIntegerConverter;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Objects;
import moscow.ptnl.contingent.domain.history.ServiceName;
import moscow.ptnl.contingent.domain.history.meta.Journalable;
import moscow.ptnl.contingent.domain.history.meta.LogIt;
import org.hibernate.annotations.Proxy;

@Entity @Journalable(ServiceName.AREA)
@Table(name = "ADDRESS_ALLOCATION_ORDERS")
@Proxy(lazy=false)
@SequenceGenerator(name = "SEQ_ADDRESS_ALLOCATION_ORDERS", sequenceName = "SEQ_ADDRESS_ALLOCATION_ORDERS", allocationSize=1)
public class AddressAllocationOrders implements Serializable {

    private static final long serialVersionUID = -8116611274341177299L;

    @Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="SEQ_ADDRESS_ALLOCATION_ORDERS")
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @LogIt
    @Size(max = 255)
    @Column(name = "NUMBER", nullable = false)
    private String number;

    @LogIt
    @Size(max = 255)
    @Column(name = "NAME")
    private String name;

    @LogIt
    @Size(max = 300)
    @Column(name = "OUZ")
    private String ouz;

    @LogIt
    @Column(name = "DATE")
    private LocalDate date;

    @LogIt
    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;

    @LogIt
    @Column(name = "CREATE_DATE", nullable = false)
    private LocalDateTime createDate;

    @LogIt
    @Column(name = "UPDATE_DATE")
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

    public Boolean getArchived() { return archived; }

    public void setArchived(Boolean archived) { this.archived = archived; }

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

    public String getOuz() {
        return ouz;
    }

    public void setOuz(String ouz) {
        this.ouz = ouz;
    }

    public boolean equalsOrd(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AddressAllocationOrders that = (AddressAllocationOrders) o;
        return Objects.equals(number, that.number) &&
                Objects.equals(name, that.name) &&
                Objects.equals(ouz, that.ouz) &&
                Objects.equals(date, that.date);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof AddressAllocationOrders) {
            return ((AddressAllocationOrders) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.id);
    }

    public static AddressAllocationOrders.Builder builder() {
        return new AddressAllocationOrders.Builder();
    }

    public static class Builder {

        private final AddressAllocationOrders addressAllocationOrders;

        private Builder(){
            this.addressAllocationOrders = new AddressAllocationOrders();
        }

        public Builder createDate(LocalDateTime createDate) {
            addressAllocationOrders.setCreateDate(createDate);
            return this;
        }

        public Builder updateDate(LocalDateTime updateDate) {
            addressAllocationOrders.setUpdateDate(updateDate);
            return this;
        }

        public Builder archived(Boolean archived) {
            addressAllocationOrders.setArchived(archived);
            return this;
        }

        public Builder number(String number) {
            addressAllocationOrders.setNumber(number);
            return this;
        }

        public Builder date(LocalDate date) {
            addressAllocationOrders.setDate(date);
            return this;
        }

        public Builder ouz(String ouz) {
            addressAllocationOrders.setOuz(ouz);
            return this;
        }

        public Builder name(String name) {
            addressAllocationOrders.setName(name);
            return this;
        }

        public AddressAllocationOrders build() {
            return addressAllocationOrders;
        }
    }
}

