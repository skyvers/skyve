package modules.admin.UserDashboard.models;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.lang.reflect.Field;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.model.chart.ChartData;

import jakarta.inject.Inject;
import modules.admin.User.UserService;
import util.AbstractH2Test;

class UserActivityModelsH2Test extends AbstractH2Test {
	@Inject
	private transient UserService userService;

	@Test
	void userActivityModelBuildsRecentActivityChartData() throws Exception {
		UserActivityModel model = new UserActivityModel();
		injectUserService(model);

		ChartData chartData = model.getChartData();

		assertThat(chartData, is(notNullValue()));
		assertThat(chartData.getTitle(), is("My activity - last 14 days"));
		assertThat(chartData.getLabel(), is("Activity"));
		assertThat(chartData.getLabels(), is(notNullValue()));
		assertThat(chartData.getValues(), is(notNullValue()));
	}

	@Test
	void userActivityContextModelBuildsRecentContextChartData() throws Exception {
		UserActivityContextModel model = new UserActivityContextModel();
		injectUserService(model);

		ChartData chartData = model.getChartData();

		assertThat(chartData, is(notNullValue()));
		assertThat(chartData.getTitle(), is("My activity by context - last 14 days"));
		assertThat(chartData.getLabel(), is("Context"));
		assertThat(chartData.getLabels(), is(notNullValue()));
		assertThat(chartData.getValues(), is(notNullValue()));
	}

	private void injectUserService(Object model) throws Exception {
		Field field = model.getClass().getDeclaredField("userService");
		field.setAccessible(true);
		field.set(model, userService);
	}
}
