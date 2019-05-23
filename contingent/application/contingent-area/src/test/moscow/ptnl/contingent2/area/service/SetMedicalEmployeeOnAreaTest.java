package moscow.ptnl.contingent2.area.service;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.PositionNomClinic;
import moscow.ptnl.contingent.area.entity.nsi.Specialization;
import moscow.ptnl.contingent.area.service.AreaServiceInternalImpl;
import moscow.ptnl.contingent.area.util.Period;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class SetMedicalEmployeeOnAreaTest {
    private static AreaServiceInternalImpl areaServiceInternal = new AreaServiceInternalImpl();
    private Specialization specialization;
    private AreaTypes areaType;
    private Area area;
    private PositionNomClinic position;
    private AreaMedicalEmployee employee1;


    @Before
    public void init() {
        specialization = new Specialization(5L, "Хирургия", false);
        areaType = new AreaTypes(3L, "школьный", false, specialization);
        area = new Area(1L, 1L, areaType, false, LocalDateTime.now());
        position = new PositionNomClinic(7L, "1", "врач-хирург", "категория", null, LocalDate.now(), null, "test", "test", "test", "test", false);
        employee1 = new AreaMedicalEmployee(1L, area, false, LocalDate.of(2019, 6, 1), null, "123", position, LocalDateTime.now(), null, 1L);
    }

    @Test
    public void getPeriodsWithoutMainEmployee_emptyEmployeesTest() {
        List<Period> periods = areaServiceInternal.getPeriodsWithoutMainEmployee(new ArrayList<>());
        Assert.assertEquals(1, periods.size());
        Assert.assertEquals(Period.ALL_TIME, periods.get(0));
    }

    @Test
    public void getPeriodsWithoutMainEmployee_singleEmployeeWithNullEndDateTest() {
        List<Period> periods = areaServiceInternal.getPeriodsWithoutMainEmployee(Collections.singletonList(employee1));
        Assert.assertEquals(1, periods.size());
        Assert.assertEquals(new Period(Period.MIN_DATE, employee1.getStartDate().minusDays(1)), periods.get(0));
    }

    @Test
    public void getPeriodsWithoutMainEmployee_singleEmployeeWithEndDateTest() {
        employee1.setEndDate(LocalDate.of(2019, 7, 1));
        List<Period> periods = areaServiceInternal.getPeriodsWithoutMainEmployee(Collections.singletonList(employee1));
        Assert.assertEquals(2, periods.size());
        Assert.assertEquals(new Period(Period.MIN_DATE, employee1.getStartDate().minusDays(1)), periods.get(0));
        Assert.assertEquals(new Period(employee1.getEndDate().plusDays(1), Period.MAX_DATE), periods.get(1));
    }
}
