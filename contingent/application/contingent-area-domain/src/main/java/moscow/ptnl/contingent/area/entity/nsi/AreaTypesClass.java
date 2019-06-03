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
@Table(name = "AREA_TYPE_CLASS")
@Cacheable
public class AreaTypesClass implements Serializable {

    private static final long serialVersionUID = 4979698890748802824L;

    @Id
	@Column(name = "CODE", unique = true, nullable = false)
	private Long code;

    @Size(max = 255)
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
    public int hashCode() {        
        return Objects.hashCode(this.code);
    }
    
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof AreaTypesClass) {
            return ((AreaTypesClass) obj).getCode().equals(this.code);
        }
        return false;
    }
}
