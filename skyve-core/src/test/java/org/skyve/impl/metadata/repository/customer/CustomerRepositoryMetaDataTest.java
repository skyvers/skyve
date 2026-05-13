package org.skyve.impl.metadata.repository.customer;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.ConverterName;

@SuppressWarnings("static-method")
class CustomerRepositoryMetaDataTest {

	// CustomerRoleMetaData

	@Test
	void customerRoleNameRoundTrips() {
		CustomerRoleMetaData r = new CustomerRoleMetaData();
		r.setName("AdminRole");
		assertThat(r.getName(), is("AdminRole"));
	}

	@Test
	void customerRoleDescriptionRoundTrips() {
		CustomerRoleMetaData r = new CustomerRoleMetaData();
		r.setDescription("Administrator role");
		assertThat(r.getDescription(), is("Administrator role"));
	}

	@Test
	void customerRoleDocumentationRoundTrips() {
		CustomerRoleMetaData r = new CustomerRoleMetaData();
		r.setDocumentation("Full docs here");
		assertThat(r.getDocumentation(), is("Full docs here"));
	}

	@Test
	void customerRoleRolesListNotNull() {
		CustomerRoleMetaData r = new CustomerRoleMetaData();
		assertThat(r.getRoles(), notNullValue());
	}

	@Test
	void customerRolePropertiesNotNull() {
		CustomerRoleMetaData r = new CustomerRoleMetaData();
		assertThat(r.getProperties(), notNullValue());
	}

	@Test
	void customerRoleCanAddModuleRoles() {
		CustomerRoleMetaData r = new CustomerRoleMetaData();
		CustomerModuleRoleMetaData mr = new CustomerModuleRoleMetaData();
		mr.setModuleName("admin");
		mr.setName("BasicUser");
		r.getRoles().add(mr);
		assertThat(r.getRoles().size(), is(1));
		assertThat(r.getRoles().get(0).getModuleName(), is("admin"));
	}

	@Test
	void customerRoleBlankDescriptionBecomesNull() {
		CustomerRoleMetaData r = new CustomerRoleMetaData();
		r.setDescription("  ");
		assertThat(r.getDescription(), nullValue());
	}

	// CustomerModuleRoleMetaData

	@Test
	void moduleRoleNameRoundTrips() {
		CustomerModuleRoleMetaData mr = new CustomerModuleRoleMetaData();
		mr.setName("ViewUser");
		assertThat(mr.getName(), is("ViewUser"));
	}

	@Test
	void moduleRoleModuleNameRoundTrips() {
		CustomerModuleRoleMetaData mr = new CustomerModuleRoleMetaData();
		mr.setModuleName("contacts");
		assertThat(mr.getModuleName(), is("contacts"));
	}

	@Test
	void moduleRoleBlankModuleNameBecomesNull() {
		CustomerModuleRoleMetaData mr = new CustomerModuleRoleMetaData();
		mr.setModuleName("");
		assertThat(mr.getModuleName(), nullValue());
	}

	// CustomerModuleMetaData

	@Test
	void customerModuleNameRoundTrips() {
		CustomerModuleMetaData cm = new CustomerModuleMetaData();
		cm.setName("admin");
		assertThat(cm.getName(), is("admin"));
	}

	@Test
	void customerModuleFormLabelLayoutRoundTrips() {
		CustomerModuleMetaData cm = new CustomerModuleMetaData();
		cm.setFormLabelLayout(FormLabelLayout.side);
		assertThat(cm.getFormLabelLayout(), is(FormLabelLayout.side));
	}

	@Test
	void customerModuleFormLabelLayoutDefaultIsNull() {
		CustomerModuleMetaData cm = new CustomerModuleMetaData();
		assertThat(cm.getFormLabelLayout(), nullValue());
	}

	// CustomerModulesMetaData

	@Test
	void customerModulesHomeModuleRoundTrips() {
		CustomerModulesMetaData m = new CustomerModulesMetaData();
		m.setHomeModule("admin");
		assertThat(m.getHomeModule(), is("admin"));
	}

	@Test
	void customerModulesListNotNull() {
		CustomerModulesMetaData m = new CustomerModulesMetaData();
		assertThat(m.getModules(), notNullValue());
	}

	@Test
	void customerModulesPropertiesNotNull() {
		CustomerModulesMetaData m = new CustomerModulesMetaData();
		assertThat(m.getProperties(), notNullValue());
	}

	@Test
	void customerModulesCanAddModule() {
		CustomerModulesMetaData m = new CustomerModulesMetaData();
		CustomerModuleMetaData cm = new CustomerModuleMetaData();
		cm.setName("admin");
		m.getModules().add(cm);
		assertThat(m.getModules().size(), is(1));
	}

	// CustomerRolesMetaData

	@Test
	void customerRolesAllowModuleRolesDefaultTrue() {
		CustomerRolesMetaData r = new CustomerRolesMetaData();
		assertThat(r.isAllowModuleRoles(), is(true));
	}

	@Test
	void customerRolesAllowModuleRolesCanBeSetFalse() {
		CustomerRolesMetaData r = new CustomerRolesMetaData();
		r.setAllowModuleRoles(false);
		assertThat(r.isAllowModuleRoles(), is(false));
	}

	@Test
	void customerRolesListNotNull() {
		CustomerRolesMetaData r = new CustomerRolesMetaData();
		assertThat(r.getRoles(), notNullValue());
	}

	// UIResources

	@Test
	void uiResourcesLogoRoundTrips() {
		UIResources u = new UIResources();
		u.setLogoRelativeFileName("images/logo.png");
		assertThat(u.getLogoRelativeFileName(), is("images/logo.png"));
	}

	@Test
	void uiResourcesLogoDefaultIsNull() {
		UIResources u = new UIResources();
		assertThat(u.getLogoRelativeFileName(), nullValue());
	}

	// InterceptorMetaDataImpl

	@Test
	void interceptorClassNameRoundTrips() {
		InterceptorMetaDataImpl i = new InterceptorMetaDataImpl();
		i.setClassName("com.example.MyInterceptor");
		assertThat(i.getClassName(), is("com.example.MyInterceptor"));
	}

	@Test
	void interceptorBlankClassNameBecomesNull() {
		InterceptorMetaDataImpl i = new InterceptorMetaDataImpl();
		i.setClassName("  ");
		assertThat(i.getClassName(), nullValue());
	}

	// CustomerMetaData basic properties

	@Test
	void customerMetaDataLanguageRoundTrips() {
		CustomerMetaData c = new CustomerMetaData();
		c.setLanguage("en");
		assertThat(c.getLanguage(), is("en"));
	}

	@Test
	void customerMetaDataDefaultDateConverterRoundTrips() {
		CustomerMetaData c = new CustomerMetaData();
		c.setDefaultDateConverter(ConverterName.DD_MMM_YYYY);
		assertThat(c.getDefaultDateConverter(), is(ConverterName.DD_MMM_YYYY));
	}

	@Test
	void customerMetaDataDefaultTimeConverterRoundTrips() {
		CustomerMetaData c = new CustomerMetaData();
		c.setDefaultTimeConverter(ConverterName.HH_MI);
		assertThat(c.getDefaultTimeConverter(), is(ConverterName.HH_MI));
	}

	@Test
	void customerMetaDataDefaultDateTimeConverterRoundTrips() {
		CustomerMetaData c = new CustomerMetaData();
		c.setDefaultDateTimeConverter(ConverterName.DD_MMM_YYYY_HH_MI);
		assertThat(c.getDefaultDateTimeConverter(), is(ConverterName.DD_MMM_YYYY_HH_MI));
	}

	@Test
	void customerMetaDataDefaultTimestampConverterRoundTrips() {
		CustomerMetaData c = new CustomerMetaData();
		c.setDefaultTimestampConverter(ConverterName.DD_MMM_YYYY_HH_MI_SS);
		assertThat(c.getDefaultTimestampConverter(), is(ConverterName.DD_MMM_YYYY_HH_MI_SS));
	}

	@Test
	void customerMetaDataInterceptorsNotNull() {
		CustomerMetaData c = new CustomerMetaData();
		assertThat(c.getInterceptors(), notNullValue());
	}

	@Test
	void customerMetaDataObserversNotNull() {
		CustomerMetaData c = new CustomerMetaData();
		assertThat(c.getObservers(), notNullValue());
	}

	@Test
	void customerMetaDataPropertiesNotNull() {
		CustomerMetaData c = new CustomerMetaData();
		assertThat(c.getProperties(), notNullValue());
	}

	@Test
	void customerMetaDataNameRoundTrips() {
		CustomerMetaData c = new CustomerMetaData();
		c.setName("skyve");
		assertThat(c.getName(), is("skyve"));
	}
}
