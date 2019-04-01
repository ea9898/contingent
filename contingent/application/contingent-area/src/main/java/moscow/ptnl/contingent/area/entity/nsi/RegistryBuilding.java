package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;

@Entity
@Table(name = "REGISTRY_BUILDING")
@Cacheable
public class RegistryBuilding implements Serializable {

    private static final long serialVersionUID = 5017009667346896559L;

    @Id
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

    @Size(max = 500)
	@Column(name = "ADDRESS_ELEMENT")
	private String addressElement;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getAddressElement() {
        return addressElement;
    }

    public void setAddressElement(String addressElement) {
        this.addressElement = addressElement;
    }
}
