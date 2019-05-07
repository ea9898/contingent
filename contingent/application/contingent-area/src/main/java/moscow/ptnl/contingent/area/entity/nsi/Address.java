package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.Objects;

@Entity
@Table(name = "ADDRESS")
@Cacheable
public class Address implements Serializable {

    private static final long serialVersionUID = -2406898213550456120L;

    @Id
    @Column(name = "ID", unique = true, nullable = false)
    private Long id;

    @Size(max = 50)
    @Column(name = "TYPE_ADDRESS", nullable = false)
    private String typeAddress;

    @Column(name = "GLOBAL_ID", nullable = false)
    private Long globalId;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getTypeAddress() {
        return typeAddress;
    }

    public void setTypeAddress(String typeAddress) {
        this.typeAddress = typeAddress;
    }

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof Address) {
            return ((Address) obj).getId().equals(this.id);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return (this.id != null) ? Objects.hashCode(this.id) : 0;
    }
}
