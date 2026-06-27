package org.skyve.metadata.module;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.user.Role;

/**
 * Tests for default methods on the Module interface.
 */
@SuppressWarnings("static-method")
class ModuleDefaultMethodsTest {

	// ---- getLocalisedTitle ----

	@Test
	void getLocalisedTitleReturnsNonNullForRealTitle() {
		ModuleImpl module = new ModuleImpl();
		module.setTitle("Admin Module");
		String result = module.getLocalisedTitle();
		assertThat(result, notNullValue());
	}

	// ---- getNullSafeMetaDataQuery ----

	@Test
	void getNullSafeMetaDataQueryReturnsQueryWhenFound() {
		Module module = spy(new ModuleImpl());
		MetaDataQueryDefinition mockQuery = mock(MetaDataQueryDefinition.class);
		when(module.getMetaDataQuery("myQuery")).thenReturn(mockQuery);
		MetaDataQueryDefinition result = module.getNullSafeMetaDataQuery("myQuery");
		assertThat(result, is(mockQuery));
	}

	@Test
	void getNullSafeMetaDataQueryThrowsWhenNotFound() {
		Module module = spy(new ModuleImpl());
		when(module.getMetaDataQuery("missingQuery")).thenReturn(null);
		when(module.getName()).thenReturn("admin");
		assertThrows(MetaDataException.class, () -> module.getNullSafeMetaDataQuery("missingQuery"));
	}

	// ---- getNullSafeRole ----

	@Test
	void getNullSafeRoleReturnsRoleWhenFound() {
		Module module = spy(new ModuleImpl());
		Role mockRole = mock(Role.class);
		when(module.getRole("AdminUser")).thenReturn(mockRole);
		Role result = module.getNullSafeRole("AdminUser");
		assertThat(result, is(mockRole));
	}

	@Test
	void getNullSafeRoleThrowsWhenNotFound() {
		Module module = spy(new ModuleImpl());
		when(module.getRole("missingRole")).thenReturn(null);
		when(module.getName()).thenReturn("admin");
		assertThrows(MetaDataException.class, () -> module.getNullSafeRole("missingRole"));
	}
}
