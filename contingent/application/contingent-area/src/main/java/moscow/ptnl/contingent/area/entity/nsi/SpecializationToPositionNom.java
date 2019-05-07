package moscow.ptnl.contingent.area.entity.nsi;

import moscow.ptnl.contingent.area.entity.converter.BooleanStrictIntegerConverter;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import java.io.Serializable;

@Entity
@Table(name = "SPECIALIZATION_TO_POSITION_NOM")
@Cacheable
public class SpecializationToPositionNom implements Serializable {

    private static final long serialVersionUID = -6560192129930420566L;

    @Id
    @JoinColumn(name = "SPECIALIZATION_ID", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private Specialization specialization;

    @Id
    @JoinColumn(name = "POSITION_NOM_ID", nullable = false)
    @ManyToOne(fetch = FetchType.LAZY)
    private PositionNom positionNom;

    @Column(name = "ARCHIVED", nullable = false)
    @Convert(converter = BooleanStrictIntegerConverter.class)
    private Boolean archived;

    public Specialization getSpecialization() {
        return specialization;
    }

    public void setSpecialization(Specialization specialization) {
        this.specialization = specialization;
    }

    public PositionNom getPositionNom() {
        return positionNom;
    }

    public void setPositionNom(PositionNom positionNom) {
        this.positionNom = positionNom;
    }

    public Boolean getArchived() {
        return archived;
    }

    public void setArchived(Boolean archived) {
        this.archived = archived;
    }
}
