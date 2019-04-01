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
@Table(name = "POSITION_NOM")
@Cacheable
public class PositionNom implements Serializable {

    private static final long serialVersionUID = -5823017631401518492L;

    @Id
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

    @Size(max = 1000)
	@Column(name = "TITLE", nullable = false)
	private String title;

    @Column(name = "ARCHIVED", nullable = false)
    private Boolean archived;

    @Column(name = "RESOURCE_TYPE_CODE")
    private Long resourceTypeCode;

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public Boolean getArchived() {
        return archived;
    }

    public void setArchived(Boolean archived) {
        this.archived = archived;
    }

    public Long getResourceTypeCode() {
        return resourceTypeCode;
    }

    public void setResourceTypeCode(Long resourceTypeCode) {
        this.resourceTypeCode = resourceTypeCode;
    }
}
