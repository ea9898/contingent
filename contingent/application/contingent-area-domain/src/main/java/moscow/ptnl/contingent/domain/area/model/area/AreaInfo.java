package moscow.ptnl.contingent.domain.area.model.area;

import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.entity.AreaMuService;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeProfile;

import java.util.List;

// Объект возвращаемый из сервисаного слоя в слой веб-сервиса
public class AreaInfo {

    private Area area;

    private AreaTypeProfile areaTypeProfile;

    private List<AreaMuService> areaMuServices;

    private List<AreaMedicalEmployees> mainAreaMedicalEmployees;

    private List<AreaMedicalEmployees> replacementAreaMedicalEmployees;

    public AreaInfo(Area area, List<AreaMedicalEmployees> mainAreaMedicalEmployees, List<AreaMedicalEmployees> replacementAreaMedicalEmployees) {
        this.area = area;
        this.mainAreaMedicalEmployees = mainAreaMedicalEmployees;
        this.replacementAreaMedicalEmployees = replacementAreaMedicalEmployees;
    }

    public AreaInfo(Area area) {
        this.area = area;
    }

    public Area getArea() {
        return area;
    }

    public void setArea(Area area) {
        this.area = area;
    }

    public AreaTypeProfile getAreaTypeProfile() {
        return areaTypeProfile;
    }

    public void setAreaTypeProfile(AreaTypeProfile areaTypeProfile) {
        this.areaTypeProfile = areaTypeProfile;
    }

    public List<AreaMuService> getAreaMuServices() {
        return areaMuServices;
    }

    public void setAreaMuServices(List<AreaMuService> areaMuServices) {
        this.areaMuServices = areaMuServices;
    }

    public List<AreaMedicalEmployees> getMainAreaMedicalEmployees() {
        return mainAreaMedicalEmployees;
    }

    public void setMainAreaMedicalEmployees(List<AreaMedicalEmployees> mainAreaMedicalEmployees) {
        this.mainAreaMedicalEmployees = mainAreaMedicalEmployees;
    }

    public List<AreaMedicalEmployees> getReplacementAreaMedicalEmployees() {
        return replacementAreaMedicalEmployees;
    }

    public void setReplacementAreaMedicalEmployees(List<AreaMedicalEmployees> replacementAreaMedicalEmployees) {
        this.replacementAreaMedicalEmployees = replacementAreaMedicalEmployees;
    }
}
