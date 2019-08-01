package service.methods;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaMedicalEmployees;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.PositionNom;
import moscow.ptnl.contingent.area.entity.nsi.Specialization;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.service.AreaServiceHelper;
import moscow.ptnl.contingent.area.util.Period;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.LoggerFactory;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

public class SetMedicalEmployeeOnAreaTest {

    private static final org.slf4j.Logger LOG = LoggerFactory.getLogger(SetMedicalEmployeeOnAreaTest.class);
    private static AreaServiceHelper areaServiceHelper = new AreaServiceHelper();
    private Specialization specialization;
    private AreaType areaType;
    private Area area1, area2;
    private Optional<PositionNom> position;
    private AreaMedicalEmployees employee1, employee2, employee3, employee4, employee5, employee6, employee7,
            employee8, employee9, employee10, employee11;
    private Validation validation;
    private Period period1, period2, period3, period4, period5, period6;

    @BeforeEach
    public void init() {
//        specialization = new Specialization(5L, "Хирургия", false);
        areaType = new AreaType(3L, "школьный", false);
        area1 = new Area(1L, 1L, 1L, areaType, false, LocalDateTime.now());
        area2 = new Area(2L, 1L, 1L, areaType, false, LocalDateTime.now());
        position = Optional.of(new PositionNom(7L, "1", "врач-хирург", LocalDate.now(), null, false));
        employee1 = new AreaMedicalEmployees(1L, 1L, area1, true, LocalDate.of(2019, 1, 10), null, "123", position, LocalDateTime.now(), null, 1L);
        employee2 = new AreaMedicalEmployees(2L, 1L, area1, false, LocalDate.of(2019, 2, 3), LocalDate.of(2019, 2, 10), "123", position, LocalDateTime.now(), null, 1L);
        employee3 = new AreaMedicalEmployees(3L, 3L, area1, false, LocalDate.of(2019, 3, 2), LocalDate.of(2019, 3, 10), "123", position, LocalDateTime.now(), null, 1L);
        employee4 = new AreaMedicalEmployees(4L, 4L, area1, false, LocalDate.of(2019, 3, 5), null, "123", position, LocalDateTime.now(), null, 1L);
        employee5 = new AreaMedicalEmployees(5L, 5L, area2, false, LocalDate.of(2019, 3, 5), LocalDate.of(2019, 3, 15), "123", position, LocalDateTime.now(), null, 1L);
        employee6 = new AreaMedicalEmployees(6L, 6L, area2, false, LocalDate.of(2019, 5, 1), LocalDate.of(2019, 5, 10), "123", position, LocalDateTime.now(), null, 1L);
        employee7 = new AreaMedicalEmployees(7L, 7L, area2, false, LocalDate.of(2019, 5, 10), LocalDate.of(2019, 5, 20), "123", position, LocalDateTime.now(), null, 1L);
        employee8 = new AreaMedicalEmployees(8L, 7L, area2, false, LocalDate.of(2019, 2, 3), LocalDate.of(2019, 2, 10), "123", position, LocalDateTime.now(), null, 1L);
        employee9 = new AreaMedicalEmployees(9L, 7L, area2, false, LocalDate.of(2019, 3, 5), LocalDate.of(2019, 3, 11), "123", position, LocalDateTime.now(), null, 1L);
        employee10 = new AreaMedicalEmployees(10L, 7L, area2, true, LocalDate.of(2019, 3, 11), LocalDate.of(2019, 3, 20), "123", position, LocalDateTime.now(), null, 1L);
        employee11 = new AreaMedicalEmployees(11L, 7L, area2, true, LocalDate.of(2019, 2, 10), LocalDate.of(2019, 3, 5), "123", position, LocalDateTime.now(), null, 1L);
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
        List<Period> periods = areaServiceHelper.getPeriodsWithoutMainEmployee(new ArrayList<>());
        Assertions.assertEquals(1, periods.size());
        Assertions.assertEquals(Period.ALL_TIME, periods.get(0));
    }

    @Test
    public void getPeriodsWithoutMainEmployee_singleEmployeeWithNullEndDateTest() {
        LOG.info("Тест getPeriodsWithoutMainEmployee передан один медработник с пустой датой окончания назначения");
        List<Period> periods = areaServiceHelper.getPeriodsWithoutMainEmployee(Collections.singletonList(employee1));
        Assertions.assertEquals(1, periods.size());
        Assertions.assertEquals(new Period(Period.MIN_DATE, employee1.getStartDate().minusDays(1)), periods.get(0));
    }

    @Test
    public void getPeriodsWithoutMainEmployee_singleEmployeeWithEndDateTest() {
        LOG.info("Тест getPeriodsWithoutMainEmployee передан один медработник с датой окончания назначения");
        List<Period> periods = areaServiceHelper.getPeriodsWithoutMainEmployee(Collections.singletonList(employee2));
        Assertions.assertEquals(2, periods.size());
        Assertions.assertEquals(new Period(Period.MIN_DATE, employee2.getStartDate().minusDays(1)), periods.get(0));
        Assertions.assertEquals(new Period(employee2.getEndDate().plusDays(1), Period.MAX_DATE), periods.get(1));
    }

    @Test
    public void getPeriodsWithoutMainEmployee_twoEmployeesWithGapTest() {
        LOG.info("Тест getPeriodsWithoutMainEmployee переданы два медработникв с датами окончания назначения");
        List<AreaMedicalEmployees> employees = Arrays.asList(employee3, employee2);
        List<Period> periods = areaServiceHelper.getPeriodsWithoutMainEmployee(employees);
        Assertions.assertEquals(3, periods.size());
        Assertions.assertEquals(new Period(Period.MIN_DATE, employee2.getStartDate().minusDays(1)), periods.get(0));
        Assertions.assertEquals(new Period(employee2.getEndDate().plusDays(1), employee3.getStartDate().minusDays(1)), periods.get(1));
        Assertions.assertEquals(new Period(employee3.getEndDate().plusDays(1), Period.MAX_DATE), periods.get(2));
    }

    @Test
    public void getPeriodsWithoutMainEmployee_threeEmployeesTest() {
        LOG.info("Тест getPeriodsWithoutMainEmployee переданы три медработника, один из которых с незаполненой датой окончания назначения");
        List<AreaMedicalEmployees> employees = Arrays.asList(employee4, employee3, employee2);
        List<Period> periods = areaServiceHelper.getPeriodsWithoutMainEmployee(employees);
        Assertions.assertEquals(2, periods.size());
        Assertions.assertEquals(new Period(Period.MIN_DATE, employee2.getStartDate().minusDays(1)), periods.get(0));
        Assertions.assertEquals(new Period(employee2.getEndDate().plusDays(1), employee3.getStartDate().minusDays(1)),
                periods.get(1));
    }

    @Test
    public void getPeriodsWithoutMainEmployee_fourEmployeesTest() {
        LOG.info("Тест getPeriodsWithoutMainEmployee переданы четыре медработника");
        List<AreaMedicalEmployees> employees = Arrays.asList(employee4, employee3, employee1, employee2);
        List<Period> periods = areaServiceHelper.getPeriodsWithoutMainEmployee(employees);
        Assertions.assertEquals(1, periods.size());
        Assertions.assertEquals(new Period(Period.MIN_DATE, employee1.getStartDate().minusDays(1)), periods.get(0));
    }

    @Test
    public void checkMainEmployeesOverlappingDates_emptyEmployeesTest() throws ContingentException {
        LOG.info("Тест checkMainEmployeesOverlappingDates передан пустой список медработников");
        areaServiceHelper.checkMainEmployeesOverlappingDates(new ArrayList<>(), validation);
    }

    @Test
    public void checkMainEmployeesOverlappingDate_singleEmployeeTest() throws ContingentException {
        LOG.info("Тест checkMainEmployeesOverlappingDates передан один медработник");
        areaServiceHelper.checkMainEmployeesOverlappingDates(Collections.singletonList(employee1), validation);
        areaServiceHelper.checkMainEmployeesOverlappingDates(Collections.singletonList(employee2), validation);
    }

    @Test
    public void checkMainEmployeesOverlappingDate_twoOverlappedEmployeesTest() {
        LOG.info("Тест checkMainEmployeesOverlappingDates переданы два пересекающихся медработника");
        try {
            areaServiceHelper.checkMainEmployeesOverlappingDates(Arrays.asList(employee4, employee3), validation);
        } catch (ContingentException e) {
            Assertions.assertEquals(1, e.getValidation().getMessages().size());
            Assertions.assertEquals(String.format(AreaErrorReason.MAIN_EMPLOYEE_DATE_OVERLAP.getDescription(),
                    employee3.getMedicalEmployeeJobId(), employee4.getMedicalEmployeeJobId()),
                    e.getValidation().getMessages().get(0).getMessage());
            return;
        }
        Assertions.fail();
    }

    @Test
    public void checkMainEmployeesOverlappingDate_twoPairOverlappedEmployeesTest() {
        LOG.info("Тест checkMainEmployeesOverlappingDates переданы две пары пересекающихся медработника");
        try {
            areaServiceHelper.checkMainEmployeesOverlappingDates(
                    Arrays.asList(employee6, employee7, employee5, employee3), validation);
        } catch (ContingentException e) {
            Assertions.assertEquals(2, e.getValidation().getMessages().size());
            Assertions.assertEquals(String.format(AreaErrorReason.MAIN_EMPLOYEE_DATE_OVERLAP.getDescription(),
                    employee3.getMedicalEmployeeJobId(), employee5.getMedicalEmployeeJobId()),
                    e.getValidation().getMessages().get(0).getMessage());
            Assertions.assertEquals(String.format(AreaErrorReason.MAIN_EMPLOYEE_DATE_OVERLAP.getDescription(),
                    employee6.getMedicalEmployeeJobId(), employee7.getMedicalEmployeeJobId()),
                    e.getValidation().getMessages().get(1).getMessage());
            return;
        }
        Assertions.fail();
    }

    @Test
    public void checkReplacementWithoutMain_emptyEmployeesTest() throws ContingentException {
        LOG.info("Тест checkReplacementWithoutMain передан пустой список медработников");
        areaServiceHelper.checkReplacementWithoutMain(
                Collections.singletonList(period1), new ArrayList<>(), validation);
    }

    @Test
    public void checkReplacementWithoutMain_emptyPeriodsTest() throws ContingentException {
        LOG.info("Тест checkReplacementWithoutMain передан пустой список периода проверки медработников");
        areaServiceHelper.checkReplacementWithoutMain(
                new ArrayList<>(), Collections.singletonList(employee1), validation);
    }

    @Test
    public void checkReplacementWithoutMain_oneEmployeeInPeriodTest() {
        LOG.info("Тест checkReplacementWithoutMain передан один медработник подпадающий в период");
        try {
            areaServiceHelper.checkReplacementWithoutMain(Collections.singletonList(period1),
                    Collections.singletonList(employee1), validation);
        } catch (ContingentException e) {
            Assertions.assertEquals(1, e.getValidation().getMessages().size());
            Assertions.assertEquals(String.format(AreaErrorReason.REPLACEMENT_WITHOUT_MAIN_EMPLOYEE.getDescription(),
                    period1.getStartDate(), period1.getEndDate()),
                    e.getValidation().getMessages().get(0).getMessage());
            return;
        }
        Assertions.fail();
    }

    @Test
    public void checkReplacementWithoutMain_severalEmployeesTest() {
        LOG.info("Тест checkReplacementWithoutMain передан один медработник подпадающий в период");
        try {
            areaServiceHelper.checkReplacementWithoutMain(Arrays.asList(period2, period3, period4),
                    Arrays.asList(employee8, employee9), validation);
        } catch (ContingentException e) {
            Assertions.assertEquals(1, e.getValidation().getMessages().size());
            Assertions.assertEquals(String.format(AreaErrorReason.REPLACEMENT_WITHOUT_MAIN_EMPLOYEE.getDescription(),
                    period4.getStartDate(), period4.getEndDate()),
                    e.getValidation().getMessages().get(0).getMessage());
            return;
        }
        Assertions.fail();
    }

    @Test
    public void Period_isInterceptWithOther() {
        LOG.info("Тест Period.isInterceptWith проверка пересечения разных периодов");
        Assertions.assertFalse(period2.isInterceptWith(period3));
        Assertions.assertFalse(period3.isInterceptWith(period2));
        Assertions.assertTrue(period1.isInterceptWith(period2));
        Assertions.assertTrue(period2.isInterceptWith(period1));
        Assertions.assertTrue(period3.isInterceptWith(period5));
        Assertions.assertTrue(period5.isInterceptWith(period3));
        Assertions.assertTrue(period5.isInterceptWith(period6));
        Assertions.assertTrue(period6.isInterceptWith(period5));
        Assertions.assertTrue(period1.isInterceptWith(period5));
        Assertions.assertTrue(period6.isInterceptWith(period1));
    }

    @Test
    public void checkDatesNotInterceptWithSamePosition_emptyEmployeesTest() throws ContingentException {
        LOG.info("Тест checkDatesNotInterceptWithSamePosition передан пустой список медработников");
        areaServiceHelper.checkDatesNotInterceptWithSamePosition(new ArrayList<>(), validation);
    }

    @Test
    public void checkDatesNotInterceptWithSamePosition_datesInterceptedForDifferentJobInfoIdEmployeesTest() throws ContingentException {
        LOG.info("Тест checkDatesNotInterceptWithSamePosition переданы медработники с разным исполнением должности и пересекающимися датами");
        areaServiceHelper.checkDatesNotInterceptWithSamePosition(
                Arrays.asList(employee3, employee4, employee5, employee6, employee7), validation);
    }

    @Test
    public void checkDatesNotInterceptWithSamePosition_datesInterceptedForSameJobInfoIdEmployeesTest() {
        LOG.info("Тест checkDatesNotInterceptWithSamePosition переданы медработники с одинаковым исполнением должности и пересекающимися датами");
        try {
            areaServiceHelper.checkDatesNotInterceptWithSamePosition(
                    Arrays.asList(employee2, employee3, employee4, employee1), validation);
        } catch (ContingentException e) {
            Assertions.assertEquals(1, e.getValidation().getMessages().size());
            Assertions.assertEquals(String.format(AreaErrorReason.JOB_ID_DATE_OVERLAP.getDescription(),
                    employee1.getMedicalEmployeeJobId(), area1.getId(), employee1.getStartDate(),
                    Period.MAX_DATE, employee2.getStartDate(), employee2.getEndDate()),
                    e.getValidation().getMessages().get(0).getMessage());
            return;
        }
        Assertions.fail();
    }

    @Test
    public void checkDatesNotInterceptWithSamePosition_datesInterceptedForSameJobInfoIdEmployeesTest2() {
        LOG.info("Тест checkDatesNotInterceptWithSamePosition переданы медработники с одинаковым исполнением должности и пересекающимися датами");
        try {
            areaServiceHelper.checkDatesNotInterceptWithSamePosition(
                    Arrays.asList(employee8, employee7, employee10, employee9, employee11), validation);
        } catch (ContingentException e) {
            Assertions.assertEquals(3, e.getValidation().getMessages().size());
            Assertions.assertEquals(String.format(AreaErrorReason.JOB_ID_DATE_OVERLAP.getDescription(),
                    employee9.getMedicalEmployeeJobId(), area2.getId(), employee8.getStartDate(),
                    employee8.getEndDate(), employee11.getStartDate(), employee11.getEndDate()),
                    e.getValidation().getMessages().get(0).getMessage());
            Assertions.assertEquals(String.format(AreaErrorReason.JOB_ID_DATE_OVERLAP.getDescription(),
                    employee9.getMedicalEmployeeJobId(), area2.getId(), employee11.getStartDate(),
                    employee11.getEndDate(), employee9.getStartDate(), employee9.getEndDate()),
                    e.getValidation().getMessages().get(1).getMessage());
            Assertions.assertEquals(String.format(AreaErrorReason.JOB_ID_DATE_OVERLAP.getDescription(),
                    employee9.getMedicalEmployeeJobId(), area2.getId(), employee9.getStartDate(),
                    employee9.getEndDate(), employee10.getStartDate(), employee10.getEndDate()),
                    e.getValidation().getMessages().get(2).getMessage());
            return;
        }
        Assertions.fail();
    }
}
