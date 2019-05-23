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
    @Column(name = "CODE", unique = true, nullable = false)
    private String name;

    @Column(name = "NAME")
    private String value;

    @Column(name = "DESCRIPTION")
    private String description;

    @Column(name = "TYPE")
    private Long type;

    @Column(name = "VAL")
    private String val;

    @Column(name = "LAST_CHANGE")
    private LocalDateTime lastChange;

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

    public Long getType() {
        return type;
    }

    public void setType(Long type) {
        this.type = type;
    }

    public String getVal() {
        return val;
    }

    public void setVal(String val) {
        this.val = val;
    }

    public LocalDateTime getLastChange() {
        return lastChange;
    }

    public void setLastChange(LocalDateTime lastChange) {
        this.lastChange = lastChange;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Setting setting = (Setting) o;
        return Objects.equals(name, setting.name) &&
                Objects.equals(value, setting.value) &&
                Objects.equals(description, setting.description) &&
                Objects.equals(type, setting.type) &&
                Objects.equals(val, setting.val);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, value, description, type, val);
    }
}

