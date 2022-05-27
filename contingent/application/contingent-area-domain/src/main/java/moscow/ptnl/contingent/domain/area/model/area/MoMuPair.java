package moscow.ptnl.contingent.domain.area.model.area;

/**
    Выходной объекта метода searchMuByAreaAddress из слоя Service
 */
public class MoMuPair {

    private Long moId;

    private Long muId;

    public MoMuPair(Long moId, Long muId) {
        this.moId = moId;
        this.muId = muId;
    }

    public Long getMoId() {
        return moId;
    }

    public void setMoId(Long moId) {
        this.moId = moId;
    }

    public Long getMuId() {
        return muId;
    }

    public void setMuId(Long muId) {
        this.muId = muId;
    }
}
