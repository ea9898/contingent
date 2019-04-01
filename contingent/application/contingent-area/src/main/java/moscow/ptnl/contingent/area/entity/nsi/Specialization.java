package moscow.ptnl.contingent.area.entity.nsi;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.io.Serializable;

@Entity
@Table(name = "SPECIALIZATION")
@Cacheable
public class Specialization implements Serializable {

    private static final long serialVersionUID = -3935499028862334002L;

    @Id
	@Column(name = "ID", unique = true, nullable = false)
	private Long id;

    @Size(max = 100)
	@Column(name = "TITLE", nullable = false)
	private String title;

	@Column(name = "ARCHIVED", nullable = false)
	private Boolean archived;

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
}
