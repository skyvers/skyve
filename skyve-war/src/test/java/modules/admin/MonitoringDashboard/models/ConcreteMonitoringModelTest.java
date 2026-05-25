package modules.admin.MonitoringDashboard.models;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.util.monitoring.RequestMeasurements;
import org.skyve.util.monitoring.ResourceMeasurements;

import modules.admin.domain.MonitoringDashboard;
import modules.admin.domain.MonitoringDashboard.Period;

/**
 * Tests the protected methods of all concrete monitoring model subclasses.
 * These tests verify the chart title/label/color strategy for each model type.
 */
@SuppressWarnings("static-method")
class ConcreteMonitoringModelTest {

	// ========== RequestElapsedTimePeriodBarModel ==========

	@Test
	void requestElapsedTimePeriodBarModelGetChartLabel() {
		RequestElapsedTimePeriodBarModel model = new RequestElapsedTimePeriodBarModel();
		assertThat(model.getChartLabel(), containsString("Elapsed Time"));
	}

	@Test
	void requestElapsedTimePeriodBarModelGetChartTitle() {
		RequestElapsedTimePeriodBarModel model = new RequestElapsedTimePeriodBarModel();
		String title = model.getChartTitle("TestRequest");
		assertThat(title, containsString("TestRequest"));
	}

	@Test
	void requestElapsedTimePeriodBarModelGetChartColour() {
		RequestElapsedTimePeriodBarModel model = new RequestElapsedTimePeriodBarModel();
		assertNotNull(model.getChartColour());
	}

	@Test
	void requestElapsedTimePeriodBarModelExtractData() {
		RequestElapsedTimePeriodBarModel model = new RequestElapsedTimePeriodBarModel();
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, ? extends Number> data = model.extractDataForTimePeriod(m, Period.currentMinute);
		assertNotNull(data);
	}

	// ========== RequestCpuUtilisationPeriodBarModel ==========

	@Test
	void requestCpuUtilisationPeriodBarModelGetChartLabel() {
		RequestCpuUtilisationPeriodBarModel model = new RequestCpuUtilisationPeriodBarModel();
		assertThat(model.getChartLabel(), containsString("CPU"));
	}

	@Test
	void requestCpuUtilisationPeriodBarModelGetChartTitle() {
		RequestCpuUtilisationPeriodBarModel model = new RequestCpuUtilisationPeriodBarModel();
		String title = model.getChartTitle("TestRequest");
		assertThat(title, containsString("TestRequest"));
	}

	@Test
	void requestCpuUtilisationPeriodBarModelGetChartColour() {
		RequestCpuUtilisationPeriodBarModel model = new RequestCpuUtilisationPeriodBarModel();
		assertNotNull(model.getChartColour());
	}

	@Test
	void requestCpuUtilisationPeriodBarModelExtractData() {
		RequestCpuUtilisationPeriodBarModel model = new RequestCpuUtilisationPeriodBarModel();
		RequestMeasurements m = new RequestMeasurements();
		Map<Integer, ? extends Number> data = model.extractDataForTimePeriod(m, Period.currentMinute);
		assertNotNull(data);
	}

	// ========== RequestHeapRamUsagePeriodBarModel ==========

	@Test
	void requestHeapRamUsagePeriodBarModelGetChartLabel() {
		RequestHeapRamUsagePeriodBarModel model = new RequestHeapRamUsagePeriodBarModel();
		assertThat(model.getChartLabel(), containsString("RAM"));
	}

	@Test
	void requestHeapRamUsagePeriodBarModelGetChartTitle() {
		RequestHeapRamUsagePeriodBarModel model = new RequestHeapRamUsagePeriodBarModel();
		assertThat(model.getChartTitle("Req"), containsString("Req"));
	}

	@Test
	void requestHeapRamUsagePeriodBarModelGetChartColour() {
		assertNotNull(new RequestHeapRamUsagePeriodBarModel().getChartColour());
	}

	@Test
	void requestHeapRamUsagePeriodBarModelExtractData() {
		RequestHeapRamUsagePeriodBarModel model = new RequestHeapRamUsagePeriodBarModel();
		RequestMeasurements m = new RequestMeasurements();
		assertNotNull(model.extractDataForTimePeriod(m, Period.currentHour));
	}

	// ========== RequestSystemCpuUsagePeriodBarModel ==========

	@Test
	void requestSystemCpuUsagePeriodBarModelGetChartLabel() {
		RequestSystemCpuUsagePeriodBarModel model = new RequestSystemCpuUsagePeriodBarModel();
		assertNotNull(model.getChartLabel());
	}

	@Test
	void requestSystemCpuUsagePeriodBarModelGetChartTitle() {
		RequestSystemCpuUsagePeriodBarModel model = new RequestSystemCpuUsagePeriodBarModel();
		assertThat(model.getChartTitle("SomeRequest"), containsString("SomeRequest"));
	}

	@Test
	void requestSystemCpuUsagePeriodBarModelGetChartColour() {
		assertNotNull(new RequestSystemCpuUsagePeriodBarModel().getChartColour());
	}

	@Test
	void requestSystemCpuUsagePeriodBarModelExtractData() {
		RequestSystemCpuUsagePeriodBarModel model = new RequestSystemCpuUsagePeriodBarModel();
		RequestMeasurements m = new RequestMeasurements();
		assertNotNull(model.extractDataForTimePeriod(m, Period.currentDay));
	}

	// ========== RequestElapsedTimeModel ==========

	@Test
	void requestElapsedTimeModelGetChartLabel() {
		RequestElapsedTimeModel model = new RequestElapsedTimeModel();
		assertNotNull(model.getChartLabel());
	}

	@Test
	void requestElapsedTimeModelGetChartTitleWithBean() {
		RequestElapsedTimeModel model = new RequestElapsedTimeModel();
		model.setBean(new MonitoringDashboard());
		assertNotNull(model.getChartTitle("myKey"));
	}

	@Test
	void requestElapsedTimeModelGetChartColor() {
		assertNotNull(new RequestElapsedTimeModel().getChartColor());
	}

	@Test
	void requestElapsedTimeModelExtractData() {
		RequestElapsedTimeModel model = new RequestElapsedTimeModel();
		RequestMeasurements m = new RequestMeasurements();
		assertNotNull(model.extractDataForTimePeriod(m, Period.currentMinute));
	}

	// ========== RequestCpuUtilisationModel ==========

	@Test
	void requestCpuUtilisationModelGetChartLabel() {
		assertNotNull(new RequestCpuUtilisationModel().getChartLabel());
	}

	@Test
	void requestCpuUtilisationModelGetChartTitleWithBean() {
		RequestCpuUtilisationModel model = new RequestCpuUtilisationModel();
		model.setBean(new MonitoringDashboard());
		assertNotNull(model.getChartTitle("reqKey"));
	}

	@Test
	void requestCpuUtilisationModelGetChartColor() {
		assertNotNull(new RequestCpuUtilisationModel().getChartColor());
	}

	@Test
	void requestCpuUtilisationModelExtractData() {
		RequestCpuUtilisationModel model = new RequestCpuUtilisationModel();
		RequestMeasurements m = new RequestMeasurements();
		assertNotNull(model.extractDataForTimePeriod(m, Period.currentMinute));
	}

	// ========== RequestHeapRamUsageModel ==========

	@Test
	void requestHeapRamUsageModelGetChartLabel() {
		assertNotNull(new RequestHeapRamUsageModel().getChartLabel());
	}

	@Test
	void requestHeapRamUsageModelGetChartTitleWithBean() {
		RequestHeapRamUsageModel model = new RequestHeapRamUsageModel();
		model.setBean(new MonitoringDashboard());
		assertNotNull(model.getChartTitle("reqKey"));
	}

	@Test
	void requestHeapRamUsageModelGetChartColor() {
		assertNotNull(new RequestHeapRamUsageModel().getChartColor());
	}

	@Test
	void requestHeapRamUsageModelExtractData() {
		RequestHeapRamUsageModel model = new RequestHeapRamUsageModel();
		RequestMeasurements m = new RequestMeasurements();
		assertNotNull(model.extractDataForTimePeriod(m, Period.currentHour));
	}

	// ========== RequestSystemCpuUsageModel ==========

	@Test
	void requestSystemCpuUsageModelGetChartLabel() {
		assertNotNull(new RequestSystemCpuUsageModel().getChartLabel());
	}

	@Test
	void requestSystemCpuUsageModelGetChartTitleWithBean() {
		RequestSystemCpuUsageModel model = new RequestSystemCpuUsageModel();
		model.setBean(new MonitoringDashboard());
		assertNotNull(model.getChartTitle("sysKey"));
	}

	@Test
	void requestSystemCpuUsageModelGetChartColor() {
		assertNotNull(new RequestSystemCpuUsageModel().getChartColor());
	}

	@Test
	void requestSystemCpuUsageModelExtractData() {
		RequestSystemCpuUsageModel model = new RequestSystemCpuUsageModel();
		RequestMeasurements m = new RequestMeasurements();
		assertNotNull(model.extractDataForTimePeriod(m, Period.currentDay));
	}

	// ========== QueryElapsedTimeModel ==========

	@Test
	void queryElapsedTimeModelGetChartLabel() {
		assertNotNull(new QueryElapsedTimeModel().getChartLabel());
	}

	@Test
	void queryElapsedTimeModelGetChartTitle() {
		QueryElapsedTimeModel model = new QueryElapsedTimeModel();
		assertThat(model.getChartTitle("admin.Users"), containsString("admin.Users"));
	}

	@Test
	void queryElapsedTimeModelGetChartColor() {
		assertNotNull(new QueryElapsedTimeModel().getChartColor());
	}

	@Test
	void queryElapsedTimeModelExtractData() {
		QueryElapsedTimeModel model = new QueryElapsedTimeModel();
		RequestMeasurements m = new RequestMeasurements();
		assertNotNull(model.extractDataForTimePeriod(m, Period.currentMinute));
	}

	// ========== QueryCpuUtilisationModel ==========

	@Test
	void queryCpuUtilisationModelGetChartLabel() {
		assertNotNull(new QueryCpuUtilisationModel().getChartLabel());
	}

	@Test
	void queryCpuUtilisationModelGetChartTitle() {
		QueryCpuUtilisationModel model = new QueryCpuUtilisationModel();
		assertThat(model.getChartTitle("admin.Reports"), containsString("admin.Reports"));
	}

	@Test
	void queryCpuUtilisationModelGetChartColor() {
		assertNotNull(new QueryCpuUtilisationModel().getChartColor());
	}

	@Test
	void queryCpuUtilisationModelExtractData() {
		QueryCpuUtilisationModel model = new QueryCpuUtilisationModel();
		RequestMeasurements m = new RequestMeasurements();
		assertNotNull(model.extractDataForTimePeriod(m, Period.currentMinute));
	}

	// ========== QueryHeapRamUsageModel ==========

	@Test
	void queryHeapRamUsageModelGetChartLabel() {
		assertNotNull(new QueryHeapRamUsageModel().getChartLabel());
	}

	@Test
	void queryHeapRamUsageModelGetChartTitle() {
		QueryHeapRamUsageModel model = new QueryHeapRamUsageModel();
		assertThat(model.getChartTitle("admin.Jobs"), containsString("admin.Jobs"));
	}

	@Test
	void queryHeapRamUsageModelGetChartColor() {
		assertNotNull(new QueryHeapRamUsageModel().getChartColor());
	}

	@Test
	void queryHeapRamUsageModelExtractData() {
		QueryHeapRamUsageModel model = new QueryHeapRamUsageModel();
		RequestMeasurements m = new RequestMeasurements();
		assertNotNull(model.extractDataForTimePeriod(m, Period.currentHour));
	}

	// ========== QuerySystemCpuUsageModel ==========

	@Test
	void querySystemCpuUsageModelGetChartLabel() {
		assertNotNull(new QuerySystemCpuUsageModel().getChartLabel());
	}

	@Test
	void querySystemCpuUsageModelGetChartTitle() {
		QuerySystemCpuUsageModel model = new QuerySystemCpuUsageModel();
		assertThat(model.getChartTitle("admin.Audits"), containsString("admin.Audits"));
	}

	@Test
	void querySystemCpuUsageModelGetChartColor() {
		assertNotNull(new QuerySystemCpuUsageModel().getChartColor());
	}

	@Test
	void querySystemCpuUsageModelExtractData() {
		QuerySystemCpuUsageModel model = new QuerySystemCpuUsageModel();
		RequestMeasurements m = new RequestMeasurements();
		assertNotNull(model.extractDataForTimePeriod(m, Period.currentDay));
	}

	// ========== DocumentCreateElapsedTimeModel ==========

	@Test
	void documentCreateElapsedTimeModelGetChartLabel() {
		assertNotNull(new DocumentCreateElapsedTimeModel().getChartLabel());
	}

	@Test
	void documentCreateElapsedTimeModelGetChartTitle() {
		DocumentCreateElapsedTimeModel model = new DocumentCreateElapsedTimeModel();
		assertThat(model.getChartTitle("User"), containsString("User"));
	}

	@Test
	void documentCreateElapsedTimeModelGetChartColor() {
		assertNotNull(new DocumentCreateElapsedTimeModel().getChartColor());
	}

	@Test
	void documentCreateElapsedTimeModelGetRequestKey() {
		DocumentCreateElapsedTimeModel model = new DocumentCreateElapsedTimeModel();
		String key = model.getRequestKey("admin.User");
		assertThat(key, is("Cadmin.User"));
	}

	@Test
	void documentCreateElapsedTimeModelExtractData() {
		DocumentCreateElapsedTimeModel model = new DocumentCreateElapsedTimeModel();
		RequestMeasurements m = new RequestMeasurements();
		assertNotNull(model.extractDataForTimePeriod(m, Period.currentMinute));
	}

	// ========== DocumentCreateCpuUtilisationModel ==========

	@Test
	void documentCreateCpuUtilisationModelGetChartLabel() {
		assertNotNull(new DocumentCreateCpuUtilisationModel().getChartLabel());
	}

	@Test
	void documentCreateCpuUtilisationModelGetChartTitle() {
		DocumentCreateCpuUtilisationModel model = new DocumentCreateCpuUtilisationModel();
		assertThat(model.getChartTitle("Document"), containsString("Document"));
	}

	@Test
	void documentCreateCpuUtilisationModelGetChartColor() {
		assertNotNull(new DocumentCreateCpuUtilisationModel().getChartColor());
	}

	@Test
	void documentCreateCpuUtilisationModelGetRequestKey() {
		DocumentCreateCpuUtilisationModel model = new DocumentCreateCpuUtilisationModel();
		String key = model.getRequestKey("admin.Document");
		assertThat(key, is("Cadmin.Document"));
	}

	// ========== DocumentCreateHeapRamUsageModel ==========

	@Test
	void documentCreateHeapRamUsageModelGetChartLabel() {
		assertNotNull(new DocumentCreateHeapRamUsageModel().getChartLabel());
	}

	@Test
	void documentCreateHeapRamUsageModelGetChartTitle() {
		DocumentCreateHeapRamUsageModel model = new DocumentCreateHeapRamUsageModel();
		assertThat(model.getChartTitle("Bean"), containsString("Bean"));
	}

	@Test
	void documentCreateHeapRamUsageModelGetRequestKey() {
		DocumentCreateHeapRamUsageModel model = new DocumentCreateHeapRamUsageModel();
		String key = model.getRequestKey("admin.Bean");
		assertThat(key, is("Cadmin.Bean"));
	}

	// ========== DocumentCreateSystemCpuUsageModel ==========

	@Test
	void documentCreateSystemCpuUsageModelGetChartLabel() {
		assertNotNull(new DocumentCreateSystemCpuUsageModel().getChartLabel());
	}

	@Test
	void documentCreateSystemCpuUsageModelGetChartTitle() {
		DocumentCreateSystemCpuUsageModel model = new DocumentCreateSystemCpuUsageModel();
		assertThat(model.getChartTitle("Entity"), containsString("Entity"));
	}

	@Test
	void documentCreateSystemCpuUsageModelGetRequestKey() {
		DocumentCreateSystemCpuUsageModel model = new DocumentCreateSystemCpuUsageModel();
		String key = model.getRequestKey("admin.Entity");
		assertThat(key, is("Cadmin.Entity"));
	}

	// ========== DocumentEditElapsedTimeModel ==========

	@Test
	void documentEditElapsedTimeModelGetChartLabel() {
		assertNotNull(new DocumentEditElapsedTimeModel().getChartLabel());
	}

	@Test
	void documentEditElapsedTimeModelGetChartTitle() {
		DocumentEditElapsedTimeModel model = new DocumentEditElapsedTimeModel();
		assertThat(model.getChartTitle("Record"), containsString("Record"));
	}

	@Test
	void documentEditElapsedTimeModelGetChartColor() {
		assertNotNull(new DocumentEditElapsedTimeModel().getChartColor());
	}

	@Test
	void documentEditElapsedTimeModelGetRequestKey() {
		DocumentEditElapsedTimeModel model = new DocumentEditElapsedTimeModel();
		String key = model.getRequestKey("admin.Record");
		assertThat(key, is("Eadmin.Record"));
	}

	// ========== DocumentEditCpuUtilisationModel ==========

	@Test
	void documentEditCpuUtilisationModelGetChartLabel() {
		assertNotNull(new DocumentEditCpuUtilisationModel().getChartLabel());
	}

	@Test
	void documentEditCpuUtilisationModelGetChartTitle() {
		DocumentEditCpuUtilisationModel model = new DocumentEditCpuUtilisationModel();
		assertThat(model.getChartTitle("Page"), containsString("Page"));
	}

	@Test
	void documentEditCpuUtilisationModelGetRequestKey() {
		DocumentEditCpuUtilisationModel model = new DocumentEditCpuUtilisationModel();
		String key = model.getRequestKey("admin.Page");
		assertThat(key, is("Eadmin.Page"));
	}

	// ========== DocumentEditHeapRamUsageModel ==========

	@Test
	void documentEditHeapRamUsageModelGetChartLabel() {
		assertNotNull(new DocumentEditHeapRamUsageModel().getChartLabel());
	}

	@Test
	void documentEditHeapRamUsageModelGetChartTitle() {
		DocumentEditHeapRamUsageModel model = new DocumentEditHeapRamUsageModel();
		assertThat(model.getChartTitle("Widget"), containsString("Widget"));
	}

	@Test
	void documentEditHeapRamUsageModelGetRequestKey() {
		DocumentEditHeapRamUsageModel model = new DocumentEditHeapRamUsageModel();
		String key = model.getRequestKey("admin.Widget");
		assertThat(key, is("Eadmin.Widget"));
	}

	// ========== DocumentEditSystemCpuUsageModel ==========

	@Test
	void documentEditSystemCpuUsageModelGetChartLabel() {
		assertNotNull(new DocumentEditSystemCpuUsageModel().getChartLabel());
	}

	@Test
	void documentEditSystemCpuUsageModelGetChartTitle() {
		DocumentEditSystemCpuUsageModel model = new DocumentEditSystemCpuUsageModel();
		assertThat(model.getChartTitle("Form"), containsString("Form"));
	}

	@Test
	void documentEditSystemCpuUsageModelGetRequestKey() {
		DocumentEditSystemCpuUsageModel model = new DocumentEditSystemCpuUsageModel();
		String key = model.getRequestKey("admin.Form");
		assertThat(key, is("Eadmin.Form"));
	}

	// ========== SystemCpuUsageModel ==========

	@Test
	void systemCpuUsageModelGetChartLabel() {
		assertNotNull(new SystemCpuUsageModel().getChartLabel());
	}

	@Test
	void systemCpuUsageModelGetChartTitleCurrentMinute() {
		assertNotNull(new SystemCpuUsageModel().getChartTitle(Period.currentMinute));
	}

	@Test
	void systemCpuUsageModelGetChartTitleCurrentHour() {
		assertNotNull(new SystemCpuUsageModel().getChartTitle(Period.currentHour));
	}

	@Test
	void systemCpuUsageModelGetChartTitleCurrentDay() {
		assertNotNull(new SystemCpuUsageModel().getChartTitle(Period.currentDay));
	}

	@Test
	void systemCpuUsageModelGetChartTitleCurrentWeek() {
		assertNotNull(new SystemCpuUsageModel().getChartTitle(Period.currentWeek));
	}

	@Test
	void systemCpuUsageModelGetChartTitleCurrentYear() {
		assertNotNull(new SystemCpuUsageModel().getChartTitle(Period.currentYear));
	}

	@Test
	void systemCpuUsageModelGetResourceDataAllPeriods() {
		SystemCpuUsageModel model = new SystemCpuUsageModel();
		ResourceMeasurements m = new ResourceMeasurements();
		assertNotNull(model.getResourceDataForPeriod(m, Period.currentMinute));
		assertNotNull(model.getResourceDataForPeriod(m, Period.currentHour));
		assertNotNull(model.getResourceDataForPeriod(m, Period.currentDay));
		assertNotNull(model.getResourceDataForPeriod(m, Period.currentWeek));
		assertNotNull(model.getResourceDataForPeriod(m, Period.currentYear));
	}

	@Test
	void systemCpuUsageModelSetChartColorsEmpty() {
		SystemCpuUsageModel model = new SystemCpuUsageModel();
		org.skyve.metadata.view.model.chart.ChartData cd = new org.skyve.metadata.view.model.chart.ChartData();
		model.setChartColors(cd, java.util.Collections.emptyList());
		assertNotNull(cd);
	}

	@Test
	void systemCpuUsageModelSetChartColorsLowLoad() {
		SystemCpuUsageModel model = new SystemCpuUsageModel();
		org.skyve.metadata.view.model.chart.ChartData cd = new org.skyve.metadata.view.model.chart.ChartData();
		model.setChartColors(cd, java.util.List.of(Float.valueOf(30f)));
		assertNotNull(cd.getBackground());
	}

	@Test
	void systemCpuUsageModelSetChartColorsMediumLoad() {
		SystemCpuUsageModel model = new SystemCpuUsageModel();
		org.skyve.metadata.view.model.chart.ChartData cd = new org.skyve.metadata.view.model.chart.ChartData();
		model.setChartColors(cd, java.util.List.of(Float.valueOf(60f)));
		assertNotNull(cd.getBackground());
	}

	@Test
	void systemCpuUsageModelSetChartColorsHighLoad() {
		SystemCpuUsageModel model = new SystemCpuUsageModel();
		org.skyve.metadata.view.model.chart.ChartData cd = new org.skyve.metadata.view.model.chart.ChartData();
		model.setChartColors(cd, java.util.List.of(Float.valueOf(75f)));
		assertNotNull(cd.getBackground());
	}

	@Test
	void systemCpuUsageModelSetChartColorsVeryHighLoad() {
		SystemCpuUsageModel model = new SystemCpuUsageModel();
		org.skyve.metadata.view.model.chart.ChartData cd = new org.skyve.metadata.view.model.chart.ChartData();
		model.setChartColors(cd, java.util.List.of(Float.valueOf(90f)));
		assertNotNull(cd.getBackground());
	}

	// ========== SystemHeapRamUsageModel ==========

	@Test
	void systemHeapRamUsageModelGetChartLabel() {
		assertNotNull(new SystemHeapRamUsageModel().getChartLabel());
	}

	@Test
	void systemHeapRamUsageModelGetChartTitleCurrentMinute() {
		assertNotNull(new SystemHeapRamUsageModel().getChartTitle(Period.currentMinute));
	}

	@Test
	void systemHeapRamUsageModelGetChartTitleCurrentDay() {
		assertNotNull(new SystemHeapRamUsageModel().getChartTitle(Period.currentDay));
	}

	@Test
	void systemHeapRamUsageModelGetResourceDataAllPeriods() {
		SystemHeapRamUsageModel model = new SystemHeapRamUsageModel();
		ResourceMeasurements m = new ResourceMeasurements();
		assertNotNull(model.getResourceDataForPeriod(m, Period.currentMinute));
		assertNotNull(model.getResourceDataForPeriod(m, Period.currentHour));
		assertNotNull(model.getResourceDataForPeriod(m, Period.currentDay));
		assertNotNull(model.getResourceDataForPeriod(m, Period.currentWeek));
		assertNotNull(model.getResourceDataForPeriod(m, Period.currentYear));
	}

	@Test
	void systemHeapRamUsageModelSetChartColorsEmpty() {
		SystemHeapRamUsageModel model = new SystemHeapRamUsageModel();
		org.skyve.metadata.view.model.chart.ChartData cd = new org.skyve.metadata.view.model.chart.ChartData();
		model.setChartColors(cd, java.util.Collections.emptyList());
		assertNotNull(cd);
	}

	@Test
	void systemHeapRamUsageModelSetChartColorsWithData() {
		SystemHeapRamUsageModel model = new SystemHeapRamUsageModel();
		org.skyve.metadata.view.model.chart.ChartData cd = new org.skyve.metadata.view.model.chart.ChartData();
		model.setChartColors(cd, java.util.List.of(Float.valueOf(50f)));
		assertNotNull(cd.getBackground());
	}

	@Test
	void systemHeapRamUsageModelSetChartColorsLowUsage() {
		SystemHeapRamUsageModel model = new SystemHeapRamUsageModel();
		org.skyve.metadata.view.model.chart.ChartData cd = new org.skyve.metadata.view.model.chart.ChartData();
		model.setChartColors(cd, java.util.List.of(Float.valueOf(30f)));
		assertNotNull(cd.getBackground());
	}

	@Test
	void systemHeapRamUsageModelSetChartColorsHighUsage() {
		SystemHeapRamUsageModel model = new SystemHeapRamUsageModel();
		org.skyve.metadata.view.model.chart.ChartData cd = new org.skyve.metadata.view.model.chart.ChartData();
		model.setChartColors(cd, java.util.List.of(Float.valueOf(75f)));
		assertNotNull(cd.getBackground());
	}

	@Test
	void systemHeapRamUsageModelSetChartColorsCriticalUsage() {
		SystemHeapRamUsageModel model = new SystemHeapRamUsageModel();
		org.skyve.metadata.view.model.chart.ChartData cd = new org.skyve.metadata.view.model.chart.ChartData();
		model.setChartColors(cd, java.util.List.of(Float.valueOf(90f)));
		assertNotNull(cd.getBackground());
	}

	// ========== getChartData() for models needing bean ==========

	@Test
	void systemCpuUsageModelGetChartDataWithBean() {
		SystemCpuUsageModel model = new SystemCpuUsageModel();
		model.setBean(new MonitoringDashboard());
		assertNotNull(model.getChartData());
	}

	@Test
	void systemHeapRamUsageModelGetChartDataWithBean() {
		SystemHeapRamUsageModel model = new SystemHeapRamUsageModel();
		model.setBean(new MonitoringDashboard());
		assertNotNull(model.getChartData());
	}

	@Test
	void queryElapsedTimeModelGetChartDataWithBean() {
		QueryElapsedTimeModel model = new QueryElapsedTimeModel();
		model.setBean(new MonitoringDashboard());
		assertNotNull(model.getChartData());
	}

	@Test
	void requestElapsedTimeModelGetChartDataWithBean() {
		RequestElapsedTimeModel model = new RequestElapsedTimeModel();
		model.setBean(new MonitoringDashboard());
		assertNotNull(model.getChartData());
	}

	@Test
	void requestElapsedTimePeriodBarModelGetChartDataWithBean() {
		RequestElapsedTimePeriodBarModel model = new RequestElapsedTimePeriodBarModel();
		model.setBean(new MonitoringDashboard());
		assertNotNull(model.getChartData());
	}

	@Test
	void documentCreateElapsedTimeModelGetChartDataWithBean() {
		DocumentCreateElapsedTimeModel model = new DocumentCreateElapsedTimeModel();
		model.setBean(new MonitoringDashboard());
		assertNotNull(model.getChartData());
	}

	// ========== AbstractMonitoringChartModel missing branches ==========

	@Test
	void getRequestDescriptionWithComponentOnlyNoModule() {
		MonitoringDashboard bean = new MonitoringDashboard();
		bean.setRsComponentName("submitForm");
		String desc = AbstractMonitoringChartModel.getRequestDescription(bean);
		assertThat(desc, containsString("submitForm"));
	}

	@Test
	void getRequestDescriptionWithModuleAndDocument() {
		MonitoringDashboard bean = new MonitoringDashboard();
		bean.setRsModuleName("admin");
		bean.setRsDocumentName("User");
		String desc = AbstractMonitoringChartModel.getRequestDescription(bean);
		assertThat(desc, containsString("admin"));
		assertThat(desc, containsString("User"));
	}

	@Test
	void getRequestDescriptionWithModuleDocumentAndComponent() {
		MonitoringDashboard bean = new MonitoringDashboard();
		bean.setRsModuleName("admin");
		bean.setRsDocumentName("User");
		bean.setRsComponentName("saveUser");
		String desc = AbstractMonitoringChartModel.getRequestDescription(bean);
		assertThat(desc, containsString("admin"));
		assertThat(desc, containsString("saveUser"));
	}

	// ========== RequestListModel utility tests ==========

	@Test
	void requestListModelGetDescriptionReturnsRequests() {
		RequestListModel model = new RequestListModel();
		assertThat(model.getDescription(), is("Requests"));
	}

	@Test
	void requestListModelGetColumnsReturnsNotNull() {
		RequestListModel model = new RequestListModel();
		assertNotNull(model.getColumns());
	}

	@Test
	void requestListModelUpdateThrowsIllegalState() throws Exception {
		RequestListModel model = new RequestListModel();
		org.junit.jupiter.api.Assertions.assertThrows(IllegalStateException.class,
				() -> model.update("someId", new java.util.TreeMap<>()));
	}

	@Test
	void requestListModelRemoveThrowsIllegalState() throws Exception {
		RequestListModel model = new RequestListModel();
		org.junit.jupiter.api.Assertions.assertThrows(IllegalStateException.class,
				() -> model.remove("someId"));
	}
}

