package moscow.ptnl.contingent.nsi.domain.area;

import jakarta.persistence.Cacheable;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Size;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.domain.annotation.MapToNsi;
import org.hibernate.annotations.Proxy;

import java.io.Serializable;
import java.time.LocalDateTime;

@Entity
@Table(name = "MEDICAL_ORGANISATIONS_ONKO")
@Cacheable
@Proxy(lazy=false)
@MapToNsi(table = NsiTablesEnum.MEDICAL_ORGANISATIONS_ONKO)
public class MedicalOrganisationsOnko implements Serializable {

    private static final long serialVersionUID = -817912141605672226L;

    @Id
    @Column(name = "GLOBAL_ID", unique = true, nullable = false)
    @Size(max = 19)
    private Long globalId;

    @Column(name = "ID", nullable = false)
    @Size(max = 38)
    private Long id;

    @Column(name = "ID_RMU")
    @Size(max = 38)
    private Long idRmu;

    @Column(name = "ID_SMVR2")
    @Size(max = 38)
    private Long id_smvr2;

    @Column(name = "NAME", nullable = false)
    @Size(max = 255)
    private String name;

    @Column(name = "TYPE", nullable = false)
    @Size(max = 255)
    private String type;

    @Column(name = "MO_ID")
    @Size(max = 38)
    private Long moId;

    @Column(name = "CODE_ONCO_AREA")
    @Size(max = 4)
    private String codeOncoArea;

    @Column(name = "ARCHIVED", nullable = false)
    private Long archived;

    @Column(name = "UPDATE_DATE", nullable = false)
    private LocalDateTime updateDate;

    @Column(name = "SOURCE")
    private String source;

    public Long getGlobalId() {
        return globalId;
    }

    public void setGlobalId(Long globalId) {
        this.globalId = globalId;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getIdRmu() {
        return idRmu;
    }

    public void setIdRmu(Long idRmu) {
        this.idRmu = idRmu;
    }

    public Long getId_smvr2() {
        return id_smvr2;
    }

    public void setId_smvr2(Long id_smvr2) {
        this.id_smvr2 = id_smvr2;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Long getMoId() {
        return moId;
    }

    public void setMoId(Long moId) {
        this.moId = moId;
    }

    public String getCodeOncoArea() {
        return codeOncoArea;
    }

    public void setCodeOncoArea(String codeOncoArea) {
        this.codeOncoArea = codeOncoArea;
    }

    public Long getArchived() {
        return archived;
    }

    public void setArchived(Long archived) {
        this.archived = archived;
    }

    public LocalDateTime getUpdateDate() {
        return updateDate;
    }

    public void setUpdateDate(LocalDateTime updateDate) {
        this.updateDate = updateDate;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }
}
