package org.skyve.metadata.module;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.Role;

/**
 * Tests for default methods on the Module interface.
 */
@ExtendWith(MockitoExtension.class)
@SuppressWarnings("static-method")
class ModuleDefaultMethodsTest {

	@Mock
	private Module mockModule;

	@Mock
	private MetaDataQueryDefinition mockQuery;

	@Mock
	private Role mockRole;

	// ---- getLocalisedTitle ----

	@Test
	void getLocalisedTitleReturnsNonNullForRealTitle() {
		when(mockModule.getTitle()).thenReturn("Admin Module");
		when(mockModule.getLocalisedTitle()).thenCallRealMethod();
		String result = mockModule.getLocalisedTitle();
		assertThat(result, notNullValue());
	}

	// ---- getNullSafeMetaDataQuery ----

	@Test
	void getNullSafeMetaDataQueryReturnsQueryWhenFound() {
		when(mockModule.getMetaDataQuery("myQuery")).thenReturn(mockQuery);
		when(mockModule.getNullSafeMetaDataQuery("myQuery")).thenCallRealMethod();
		MetaDataQueryDefinition result = mockModule.getNullSafeMetaDataQuery("myQuery");
		assertThat(result, is(mockQuery));
	}

	@Test
	void getNullSafeMetaDataQueryThrowsWhenNotFound() {
		when(mockModule.getMetaDataQuery("missingQuery")).thenReturn(null);
		when(mockModule.getName()).thenReturn("admin");
		when(mockModule.getNullSafeMetaDataQuery("missingQuery")).thenCallRealMethod();
		assertThrows(MetaDataException.class, () -> mockModule.getNullSafeMetaDataQuery("missingQuery"));
	}

	// ---- getNullSafeRole ----

	@Test
	void getNullSafeRoleReturnsRoleWhenFound() {
		when(mockModule.getRole("AdminUser")).thenReturn(mockRole);
		when(mockModule.getNullSafeRole("AdminUser")).thenCallRealMethod();
		Role result = mockModule.getNullSafeRole("AdminUser");
		assertThat(result, is(mockRole));
	}

	@Test
	void getNullSafeRoleThrowsWhenNotFound() {
		when(mockModule.getRole("missingRole")).thenReturn(null);
		when(mockModule.getName()).thenReturn("admin");
		when(mockModule.getNullSafeRole("missingRole")).thenCallRealMethod();
		assertThrows(MetaDataException.class, () -> mockModule.getNullSafeRole("missingRole"));
	}
}
