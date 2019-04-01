package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;

@Entity
@Table(name = "ADDRESS_FORMING_ELEMENT")
@Cacheable
public class AddressFormingElement implements Serializable {

    private static final long serialVersionUID = 5085904638016789913L;

    @Id
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

    @Size(max = 50)
	@Column(name = "TYPE")
	private String type;

    @Size(max = 350)
	@Column(name = "NAME")
	private String name;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
