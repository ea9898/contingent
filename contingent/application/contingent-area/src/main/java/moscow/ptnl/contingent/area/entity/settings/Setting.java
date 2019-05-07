package moscow.ptnl.contingent.area.entity.settings;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;
import javax.persistence.Cacheable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "AD_CONFIG")
@Cacheable
public class Setting implements Serializable {

    private static final long serialVersionUID = -7504397229834601830L;

    @Id
    @Column(name = "NAME", unique = true, nullable = false)
    private String name;

    @Column(name = "VAL")
    private String value;

    @Column(name = "DESCRIPTION")
    private String description;

    @Column(name = "LAST_CHANGE")
    private LocalDateTime lastChange;

    @Column(name = "USER_ID")
    private String iserId;

    @Column(name = "VAL_TYPE")
    private Short valType;

    @Column(name = "IS_EXT_VIEW")
    private Boolean isExtView;

    public String getName() {
            return name;
    }

    public void setName(String name) {
            this.name = name;
    }

    public String getValue() {
            return value;
    }

    public void setValue(String value) {
            this.value = value;
    }

    public String getDescription() {
            return description;
    }

    public void setDescription(String description) {
            this.description = description;
    }

    public LocalDateTime getLastChange() {
            return lastChange;
    }

    public void setLastChange(LocalDateTime lastChange) {
            this.lastChange = lastChange;
    }

    public String getIserId() {
            return iserId;
    }

    public void setIserId(String iserId) {
            this.iserId = iserId;
    }

    public Short getValType() {
            return valType;
    }

    public void setValType(Short valType) {
            this.valType = valType;
    }

    public Boolean getIsExtView() {
            return isExtView;
    }

    public void setIsExtView(Boolean isExtView) {
            this.isExtView = isExtView;
    }

    public Boolean getExtView() {
            return isExtView;
    }

    public void setExtView(Boolean extView) {
            isExtView = extView;
    }
        
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj != null && obj instanceof Setting) {
            return ((Setting) obj).getName().equals(this.name);
        }
        return false;
    }

    @Override
    public int hashCode() {        
        return Objects.hashCode(this.name);
    }
}
