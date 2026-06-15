package modules.admin.SystemDashboard.models;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.model.chart.ChartData;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class SystemDashboardActivityModelsH2Test extends AbstractH2Test {
	@Test
	void activityModelBuildsRecentActivityChartData() {
		ChartData chartData = new ActivityModel().getChartData();

		assertThat(chartData, is(notNullValue()));
		assertThat(chartData.getTitle(), is("System activity - last 14 days"));
		assertThat(chartData.getLabel(), is("Activity"));
		assertThat(chartData.getLabels(), is(notNullValue()));
		assertThat(chartData.getValues(), is(notNullValue()));
	}

	@Test
	void activityContextModelBuildsRecentContextChartData() {
		ChartData chartData = new ActivityContextModel().getChartData();

		assertThat(chartData, is(notNullValue()));
		assertThat(chartData.getTitle(), is("System activity by context - last 14 days"));
		assertThat(chartData.getLabel(), is("Context"));
		assertThat(chartData.getLabels(), is(notNullValue()));
		assertThat(chartData.getValues(), is(notNullValue()));
	}
}
