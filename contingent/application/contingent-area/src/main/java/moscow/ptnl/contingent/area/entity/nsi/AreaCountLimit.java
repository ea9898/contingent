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
@Table(name = "AREA_COUNT_LIMIT")
@Cacheable
public class AreaCountLimit implements Serializable {

    private static final long serialVersionUID = 1173406390214884871L;

    @Id
    @Column(name = "CODE", unique = true, nullable = false)
    private Long code;

    @Size(max = 50)
    @Column(name = "VALUE")
    private String value;

    public Long getCode() {
        return code;
    }

    public void setCode(Long code) {
        this.code = code;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof AreaCountLimit) {
            return ((AreaCountLimit) obj).getCode().equals(this.code);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return (this.code != null) ? Objects.hashCode(this.code) : 0;
    }
}
