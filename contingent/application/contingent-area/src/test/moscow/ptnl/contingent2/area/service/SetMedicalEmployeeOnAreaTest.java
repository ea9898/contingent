package moscow.ptnl.contingent2.area.service;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployee;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.PositionNomClinic;
import moscow.ptnl.contingent.area.entity.nsi.Specialization;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.service.AreaServiceInternalImpl;
import moscow.ptnl.contingent.area.util.Period;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.LoggerFactory;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class SetMedicalEmployeeOnAreaTest {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(SetMedicalEmployeeOnAreaTest.class);
    private static AreaServiceInternalImpl areaServiceInternal = new AreaServiceInternalImpl();
    private Specialization specialization;
    private AreaTypes areaType;
    private Area area;
    private PositionNomClinic position;
    private AreaMedicalEmployee employee1, employee2, employee3, employee4, employee5, employee6, employee7, employee8, employee9;
    private List<AreaMedicalEmployee> employees;
    private Validation validation;
    private Period period1, period2, period3, period4,period5,period6;

    @Before
    public void init() {
        specialization = new Specialization(5L, "Хирургия", false);
        areaType = new AreaTypes(3L, "школьный", false, specialization);
        area = new Area(1L, 1L, areaType, false, LocalDateTime.now());
        position = new PositionNomClinic(7L, "1", "врач-хирург", "категория", null, LocalDate.now(), null, "test", "test", "test", "test", false);
        employee1 = new AreaMedicalEmployee(1L,1L, area, true, LocalDate.of(2019, 1, 10), null, "123", position, LocalDateTime.now(), null, 1L);
        employee2 = new AreaMedicalEmployee(2L,1L, area, false, LocalDate.of(2019, 2, 3), LocalDate.of(2019, 2, 10), "123", position, LocalDateTime.now(), null, 1L);
        employee3 = new AreaMedicalEmployee(3L,3L, area, false, LocalDate.of(2019, 3, 2), LocalDate.of(2019, 3, 10), "123", position, LocalDateTime.now(), null, 1L);
        employee4 = new AreaMedicalEmployee(4L,4L, area, false, LocalDate.of(2019, 3, 5), null, "123", position, LocalDateTime.now(), null, 1L);
        employee5 = new AreaMedicalEmployee(5L,5L, area, false, LocalDate.of(2019, 3, 5), LocalDate.of(2019, 3, 15), "123", position, LocalDateTime.now(), null, 1L);
        employee6 = new AreaMedicalEmployee(6L,6L, area, false, LocalDate.of(2019, 5, 1), LocalDate.of(2019, 5, 10), "123", position, LocalDateTime.now(), null, 1L);
        employee7 = new AreaMedicalEmployee(7L,7L, area, false, LocalDate.of(2019, 5, 10), LocalDate.of(2019, 5, 20), "123", position, LocalDateTime.now(), null, 1L);
        employee8 = new AreaMedicalEmployee(8L,7L, area, false, LocalDate.of(2019, 2, 3),LocalDate.of(2019, 2, 10), "123", position, LocalDateTime.now(), null, 1L);
        employee9 = new AreaMedicalEmployee(9L,7L, area, false, LocalDate.of(2019, 3, 5), LocalDate.of(2019, 3, 11), "123", position, LocalDateTime.now(), null, 1L);
        employees = new ArrayList<>();
        validation = new Validation();
        period1 = Period.ALL_TIME;
        period2 = new Period(Period.MIN_DATE, LocalDate.of(2019, 2, 2));
        period3 = new Period(LocalDate.of(2019, 2, 11), LocalDate.of(2019, 3, 1));
        period4 = new Period(LocalDate.of(2019, 3, 11), Period.MAX_DATE);
        period5 = new Period(LocalDate.of(2019, 3, 1), LocalDate.of(2019, 3, 5));
        period6 = new Period(LocalDate.of(2019, 2, 1), LocalDate.of(2019, 3, 1));
    }

    @Test
    public void getPeriodsWithoutMainEmployee_emptyEmployeesTest() {
        LOG.info("Тест getPeriodsWithoutMainEmployee передан пустой список медработников");
        List<Period> periods = areaServiceInternal.getPeriodsWithoutMainEmployee(new ArrayList<>());
        Assert.assertEquals(1, periods.size());
        Assert.assertEquals(Period.ALL_TIME, periods.get(0));
    }

    @Test
    public void getPeriodsWithoutMainEmployee_singleEmployeeWithNullEndDateTest() {
        LOG.info("Тест getPeriodsWithoutMainEmployee передан один медработник с пустой датой окончания назначения");
        List<Period> periods = areaServiceInternal.getPeriodsWithoutMainEmployee(Collections.singletonList(employee1));
        Assert.assertEquals(1, periods.size());
        Assert.assertEquals(new Period(Period.MIN_DATE, employee1.getStartDate().minusDays(1)), periods.get(0));
    }

    @Test
    public void getPeriodsWithoutMainEmployee_singleEmployeeWithEndDateTest() {
        LOG.info("Тест getPeriodsWithoutMainEmployee передан один медработник с датой окончания назначения");
        List<Period> periods = areaServiceInternal.getPeriodsWithoutMainEmployee(Collections.singletonList(employee2));
        Assert.assertEquals(2, periods.size());
        Assert.assertEquals(new Period(Period.MIN_DATE, employee2.getStartDate().minusDays(1)), periods.get(0));
        Assert.assertEquals(new Period(employee2.getEndDate().plusDays(1), Period.MAX_DATE), periods.get(1));
    }

    @Test
    public void getPeriodsWithoutMainEmployee_twoEmployeesWithGapTest() {
        LOG.info("Тест getPeriodsWithoutMainEmployee переданы два медработникв с датами окончания назначения");
        List<AreaMedicalEmployee> employees = Arrays.asList(employee3, employee2);
        List<Period> periods = areaServiceInternal.getPeriodsWithoutMainEmployee(employees);
        Assert.assertEquals(3, periods.size());
        Assert.assertEquals(new Period(Period.MIN_DATE, employee2.getStartDate().minusDays(1)), periods.get(0));
        Assert.assertEquals(new Period(employee2.getEndDate().plusDays(1), employee3.getStartDate().minusDays(1)), periods.get(1));
        Assert.assertEquals(new Period(employee3.getEndDate().plusDays(1), Period.MAX_DATE), periods.get(2));
    }

    @Test
    public void getPeriodsWithoutMainEmployee_threeEmployeesTest() {
        LOG.info("Тест getPeriodsWithoutMainEmployee переданы три медработника, один из которых с незаполненой датой окончания назначения");
        List<AreaMedicalEmployee> employees = Arrays.asList(employee4, employee3, employee2);
        List<Period> periods = areaServiceInternal.getPeriodsWithoutMainEmployee(employees);
        Assert.assertEquals(2, periods.size());
        Assert.assertEquals(new Period(Period.MIN_DATE, employee2.getStartDate().minusDays(1)), periods.get(0));
        Assert.assertEquals(new Period(employee2.getEndDate().plusDays(1), employee3.getStartDate().minusDays(1)), periods.get(1));
    }

    @Test
    public void getPeriodsWithoutMainEmployee_fourEmployeesTest() {
        LOG.info("Тест getPeriodsWithoutMainEmployee переданы четыре медработника");
        List<AreaMedicalEmployee> employees = Arrays.asList(employee4, employee3, employee1, employee2);
        List<Period> periods = areaServiceInternal.getPeriodsWithoutMainEmployee(employees);
        Assert.assertEquals(1, periods.size());
        Assert.assertEquals(new Period(Period.MIN_DATE, employee1.getStartDate().minusDays(1)), periods.get(0));
    }

    @Test
    public void checkMainEmployeesOverlappingDates_emptyEmployeesTest() throws ContingentException {
        LOG.info("Тест checkMainEmployeesOverlappingDates передан пустой список медработников");
        areaServiceInternal.checkMainEmployeesOverlappingDates(new ArrayList<>(), validation);
    }

    @Test
    public void checkMainEmployeesOverlappingDate_singleEmployeeTest() throws ContingentException {
        LOG.info("Тест checkMainEmployeesOverlappingDates передан один медработник");
        areaServiceInternal.checkMainEmployeesOverlappingDates(Collections.singletonList(employee1), validation);
        areaServiceInternal.checkMainEmployeesOverlappingDates(Collections.singletonList(employee2), validation);
    }

    @Test
    public void checkMainEmployeesOverlappingDate_twoOverlappedEmployeesTest() {
        LOG.info("Тест checkMainEmployeesOverlappingDates переданы два пересекающихся медработника");
        try {
            areaServiceInternal.checkMainEmployeesOverlappingDates(Arrays.asList(employee4, employee3), validation);
        } catch (ContingentException e) {
            Assert.assertEquals(1, e.getValidation().getMessages().size());
            Assert.assertEquals(String.format(AreaErrorReason.MAIN_EMPLOYEE_DATE_OVERLAP.getDescription(),
                    employee3.getMedicalEmployeeJobInfoId(), employee4.getMedicalEmployeeJobInfoId()),
                    e.getValidation().getMessages().get(0).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void checkMainEmployeesOverlappingDate_twoPairOverlappedEmployeesTest() throws ContingentException {
        LOG.info("Тест checkMainEmployeesOverlappingDates переданы две пары пересекающихся медработника");
        try {
            areaServiceInternal.checkMainEmployeesOverlappingDates(
                    Arrays.asList(employee6, employee7, employee5, employee3), validation);
        } catch (ContingentException e) {
            Assert.assertEquals(2, e.getValidation().getMessages().size());
            Assert.assertEquals(String.format(AreaErrorReason.MAIN_EMPLOYEE_DATE_OVERLAP.getDescription(),
                    employee3.getMedicalEmployeeJobInfoId(), employee5.getMedicalEmployeeJobInfoId()),
                    e.getValidation().getMessages().get(0).getMessage());
            Assert.assertEquals(String.format(AreaErrorReason.MAIN_EMPLOYEE_DATE_OVERLAP.getDescription(),
                    employee6.getMedicalEmployeeJobInfoId(), employee7.getMedicalEmployeeJobInfoId()),
                    e.getValidation().getMessages().get(1).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void checkReplacementWithoutMain_emptyEmployeesTest() throws ContingentException {
        LOG.info("Тест checkReplacementWithoutMain передан пустой список медработников");
        areaServiceInternal.checkReplacementWithoutMain(Collections.singletonList(period1), new ArrayList<>(), validation);
    }

    @Test
    public void checkReplacementWithoutMain_emptyPeriodsTest() throws ContingentException {
        LOG.info("Тест checkReplacementWithoutMain передан пустой список периода проверки медработников");
        areaServiceInternal.checkReplacementWithoutMain(new ArrayList<>(), Collections.singletonList(employee1), validation);
    }

    @Test
    public void checkReplacementWithoutMain_oneEmployeeInPeriodTest()  {
        LOG.info("Тест checkReplacementWithoutMain передан один медработник подпадающий в период");
        try {
            areaServiceInternal.checkReplacementWithoutMain(Collections.singletonList(period1),
                    Collections.singletonList(employee1), validation);
        } catch (ContingentException e) {
            Assert.assertEquals(1, e.getValidation().getMessages().size());
            Assert.assertEquals(String.format(AreaErrorReason.REPLACEMENT_WITHOUT_MAIN_EMPLOYEE.getDescription(),
                    period1.getStartDate(), period1.getEndDate()),
                    e.getValidation().getMessages().get(0).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void checkReplacementWithoutMain_severalEmployeesTest(){
        LOG.info("Тест checkReplacementWithoutMain передан один медработник подпадающий в период");
        try {
            areaServiceInternal.checkReplacementWithoutMain(Arrays.asList(period2,period3,period4),
                    Arrays.asList(employee8, employee9), validation);
        } catch (ContingentException e) {
            Assert.assertEquals(1, e.getValidation().getMessages().size());
            Assert.assertEquals(String.format(AreaErrorReason.REPLACEMENT_WITHOUT_MAIN_EMPLOYEE.getDescription(),
                    period4.getStartDate(), period4.getEndDate()),
                    e.getValidation().getMessages().get(0).getMessage());
            return;
        }
        Assert.fail();
    }

    @Test
    public void Period_isInterceptWithOther() {
        LOG.info("Тест Period.isInterceptWith проверка метода isInterceptWith");
        Assert.assertFalse(period2.isInterceptWith(period3));
        Assert.assertFalse(period3.isInterceptWith(period2));
        Assert.assertTrue(period1.isInterceptWith(period2));
        Assert.assertTrue(period2.isInterceptWith(period1));
        Assert.assertTrue(period3.isInterceptWith(period5));
        Assert.assertTrue(period5.isInterceptWith(period3));
        Assert.assertTrue(period5.isInterceptWith(period6));
        Assert.assertTrue(period6.isInterceptWith(period5));
        Assert.assertTrue(period1.isInterceptWith(period5));
        Assert.assertTrue(period6.isInterceptWith(period1));
    }

    //checkDatesNotInterceptWithSamePosition

}
