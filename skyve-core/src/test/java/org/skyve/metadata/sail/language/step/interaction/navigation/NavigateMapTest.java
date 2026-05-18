package org.skyve.metadata.sail.language.step.interaction.navigation;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.metadata.sail.execution.Executor;

class NavigateMapTest {

	private NavigateMap step;

	@BeforeEach
	void setUp() {
		step = new NavigateMap();
	}

	// ---- fields from NavigateMap itself ----

	@Test
	void defaultGeometryBindingIsNull() {
		assertThat(step.getGeometryBinding(), is(nullValue()));
	}

	@Test
	void defaultRefreshTimeIsNull() {
		assertThat(step.getRefreshTimeInSeconds(), is(nullValue()));
	}

	@Test
	void defaultShowRefreshControlsIsNull() {
		assertThat(step.getShowRefreshControls(), is(nullValue()));
	}

	@Test
	void setGeometryBindingRoundtrip() {
		step.setGeometryBinding("location");
		assertThat(step.getGeometryBinding(), is("location"));
	}

	@Test
	void setGeometryBindingTrimsWhitespace() {
		step.setGeometryBinding("  loc  ");
		assertThat(step.getGeometryBinding(), is("loc"));
	}

	@Test
	void setGeometryBindingBlankBecomesNull() {
		step.setGeometryBinding("   ");
		assertThat(step.getGeometryBinding(), is(nullValue()));
	}

	@Test
	void setRefreshTimeRoundtrip() {
		step.setRefreshTimeInSeconds(Integer.valueOf(30));
		assertThat(step.getRefreshTimeInSeconds(), is(Integer.valueOf(30)));
	}

	@Test
	void setShowRefreshControlsRoundtrip() {
		step.setShowRefreshControls(Boolean.TRUE);
		assertThat(step.getShowRefreshControls(), is(Boolean.TRUE));
	}

	// ---- inherited fields from NavigateList ----

	@Test
	void setModuleNameRoundtrip() {
		step.setModuleName("admin");
		assertThat(step.getModuleName(), is("admin"));
	}

	@Test
	void setDocumentNameRoundtrip() {
		step.setDocumentName("Location");
		assertThat(step.getDocumentName(), is("Location"));
	}

	@Test
	void executeCallsExecutor() {
		Executor executor = mock(Executor.class);
		step.execute(executor);
		verify(executor).executeNavigateMap(step);
	}
}
