package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;

@Entity
@Table(name = "KIND_AREA_TYPES")
@Cacheable
public class KindAreaTypes implements Serializable {

    private static final long serialVersionUID = 7174737671670446575L;

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
}
