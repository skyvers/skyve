package org.skyve.metadata.module;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Tests for default methods on the JobMetaData interface.
 */
@ExtendWith(MockitoExtension.class)
@SuppressWarnings("static-method")
class JobMetaDataDefaultMethodsTest {

	@Mock
	private JobMetaData mockJob;

	@Test
	void getLocalisedDisplayNameReturnsNullForNullDisplayName() {
		when(mockJob.getDisplayName()).thenReturn(null);
		when(mockJob.getLocalisedDisplayName()).thenCallRealMethod();
		// Util.i18n(null) returns null
		assertThat(mockJob.getLocalisedDisplayName(), nullValue());
	}

	@Test
	void getLocalisedDisplayNameReturnsNonNullForRealName() {
		when(mockJob.getDisplayName()).thenReturn("Generate Report");
		when(mockJob.getLocalisedDisplayName()).thenCallRealMethod();
		assertThat(mockJob.getLocalisedDisplayName(), notNullValue());
	}

	@Test
	void getLocalisedDescriptionReturnsNullForNullDescription() {
		when(mockJob.getDescription()).thenReturn(null);
		when(mockJob.getLocalisedDescription()).thenCallRealMethod();
		// Util.i18n(null) returns null
		assertThat(mockJob.getLocalisedDescription(), nullValue());
	}

	@Test
	void getLocalisedDescriptionReturnsNonNullForRealDescription() {
		when(mockJob.getDescription()).thenReturn("Generates a weekly report");
		when(mockJob.getLocalisedDescription()).thenCallRealMethod();
		assertThat(mockJob.getLocalisedDescription(), notNullValue());
	}
}
