package org.skyve.metadata.customer.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.converters.Converter;
import org.skyve.impl.metadata.repository.customer.CustomerFeatureRoleMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerModuleRoleMetaData;
import org.skyve.impl.metadata.repository.customer.CustomerRoleMetaData;
import org.skyve.impl.metadata.repository.customer.HTMLResourcesMetaData;
import org.skyve.impl.metadata.repository.customer.InterceptorMetaDataImpl;
import org.skyve.impl.metadata.repository.customer.LoginResourcesMetaData;
import org.skyve.impl.metadata.repository.customer.ObserverMetaDataImpl;
import org.skyve.impl.metadata.repository.customer.UIResources;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.ConverterName;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;

/** Exercises fluent customer builders for nested roles, modules, resources, and copy paths. */
@SuppressWarnings("static-method")
class FluentCustomerTest {
	/** Verifies that supporting fluent customer helper types copy their source metadata values. */
	@Test
	void supportingFluentTypesCopyValues() {
		LoginResourcesMetaData login = new LoginResourcesMetaData();
		login.setLoginPageURL("/login");
		login.setLoggedOutPageURL("/logout");
		FluentLoginResources fluentLogin = new FluentLoginResources().from(login);
		assertThat(fluentLogin.get().getLoginPageURL(), is("/login"));
		assertThat(fluentLogin.get().getLoggedOutPageURL(), is("/logout"));

		CustomerFeatureRoleMetaData featureRole = new CustomerFeatureRoleMetaData();
		featureRole.setModuleName("sales");
		featureRole.setName("SearchOrders");
		FluentCustomerFeatureRole fluentFeatureRole = new FluentCustomerFeatureRole().from(featureRole);
		assertThat(fluentFeatureRole.get().getModuleName(), is("sales"));
		assertThat(fluentFeatureRole.get().getName(), is("SearchOrders"));

		CustomerModuleRoleMetaData moduleRole = new CustomerModuleRoleMetaData();
		moduleRole.setModuleName("sales");
		moduleRole.setName("Manager");
		FluentCustomerModuleRole fluentModuleRole = new FluentCustomerModuleRole().from(moduleRole);
		assertThat(fluentModuleRole.get().getModuleName(), is("sales"));
		assertThat(fluentModuleRole.get().getName(), is("Manager"));
	}

	/** Verifies that customer roles can copy, find, remove, and clear nested roles. */
	@Test
	void customerRoleAndRolesManageNestedRoles() {
		CustomerModuleRoleMetaData sourceModuleRole = new CustomerModuleRoleMetaData();
		sourceModuleRole.setModuleName("sales");
		sourceModuleRole.setName("Manager");
		CustomerRoleMetaData sourceRole = new CustomerRoleMetaData();
		sourceRole.setName("Admin");
		sourceRole.setDescription("Admin role");
		sourceRole.setDocumentation("Docs");
		sourceRole.getRoles().add(sourceModuleRole);

		FluentCustomerRole fluentRole = new FluentCustomerRole().from(sourceRole);
		assertThat(fluentRole.findRole("sales", "Manager"), is(notNullValue()));
		fluentRole.removeRole("sales", "Manager");
		assertThat(fluentRole.findRole("sales", "Manager"), is(nullValue()));
		fluentRole.addRole(new FluentCustomerModuleRole().moduleName("admin").name("Auditor"));
		assertThat(fluentRole.findRole("admin", "Auditor"), is(notNullValue()));
		fluentRole.clearRoles();
		assertTrue(fluentRole.get().getRoles().isEmpty());

		FluentCustomerRoles fluentRoles = new FluentCustomerRoles().allowModuleRoles(true).addRole(fluentRole);
		assertTrue(fluentRoles.get().isAllowModuleRoles());
		assertThat(fluentRoles.findRole("Admin"), is(notNullValue()));
		fluentRoles.removeRole("Admin");
		assertThat(fluentRoles.findRole("Admin"), is(nullValue()));
		fluentRoles.from(List.of(sourceRole), false);
		assertFalse(fluentRoles.get().isAllowModuleRoles());
		assertThat(fluentRoles.findRole("Admin"), is(notNullValue()));
		fluentRoles.clearRoles();
		assertTrue(fluentRoles.get().getRoles().isEmpty());
	}

	/** Verifies that fluent customer modules manage module entries and the home module value. */
	@Test
	void customerModulesManageModulesAndHomeModule() {
		Module sales = mock(Module.class);
		when(sales.getName()).thenReturn("sales");
		when(sales.getFormLabelLayout()).thenReturn(FormLabelLayout.side);
		Module admin = mock(Module.class);
		when(admin.getName()).thenReturn("admin");
		when(admin.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		FluentCustomerModules modules = new FluentCustomerModules().homeModule("sales")
				.addModule("sales", FormLabelLayout.side)
				.addModule("admin");
		assertEquals(2, modules.get().getModules().size());
		modules.removeModule("sales");
		assertThat(modules.get().getHomeModule(), is(nullValue()));
		modules.from(List.of(sales, admin), admin);
		assertThat(modules.get().getHomeModule(), is("admin"));
		assertEquals(3, modules.get().getModules().size());
		modules.clearModules();
		assertTrue(modules.get().getModules().isEmpty());
	}

	/** Verifies that fluent customer mutators manage roles, hooks, and UI resource metadata. */
	@Test
	void customerMutatorsManageRolesHooksAndResources() {
		FluentCustomer customer = new FluentCustomer().name("bizhub")
				.language("en-AU")
				.logoRelativeFileName("logo.svg")
				.cssRelativeFileName("theme.css")
				.loginResource(new FluentLoginResources().loginPageURL("/login").loggedOutPageURL("/logout"))
				.defaultDateConverter(ConverterName.DD_MM_YYYY)
				.defaultTimeConverter(ConverterName.HH24_MI)
				.defaultDateTimeConverter(ConverterName.DD_MM_YYYY_HH24_MI)
				.defaultTimestampConverter(ConverterName.DD_MM_YYYY_HH24_MI_SS)
				.modules(new FluentCustomerModules().homeModule("sales").addModule("sales"))
				.roles(new FluentCustomerRoles().allowModuleRoles(true).addRole(new FluentCustomerRole().name("Admin")))
				.addTextSearchRole(new FluentCustomerFeatureRole().moduleName("sales").name("SearchOrders"))
				.addTextSearchRole(new FluentCustomerFeatureRole().name("SearchAll"))
				.addFlagRole(new FluentCustomerFeatureRole().moduleName("sales").name("FlagOrders"))
				.addFlagRole(new FluentCustomerFeatureRole().name("FlagAll"))
				.addSwitchModeRole(new FluentCustomerFeatureRole().moduleName("sales").name("SwitchOrders"))
				.addSwitchModeRole(new FluentCustomerFeatureRole().name("SwitchAll"))
				.addInterceptor("example.Interceptor")
				.addObserver("example.Observer")
				.jFreeChartPostProcessorClassName("charts.JFree")
				.primeFacesChartPostProcessorClassName("charts.PrimeFaces");

		assertThat(customer.findTextSearchModuleRole("sales", "SearchOrders"), is(notNullValue()));
		assertThat(customer.findTextSearchCustomerRole("SearchAll"), is(notNullValue()));
		assertThat(customer.findFlagModuleRole("sales", "FlagOrders"), is(notNullValue()));
		assertThat(customer.findFlagCustomerRole("FlagAll"), is(notNullValue()));
		assertThat(customer.findSwitchModeModuleRole("sales", "SwitchOrders"), is(notNullValue()));
		assertThat(customer.findSwitchModeCustomerRole("SwitchAll"), is(notNullValue()));
		assertThat(customer.get().getUiResources().getLogoRelativeFileName(), is("logo.svg"));
		assertThat(customer.get().getHtmlResources().getCssRelativeFileName(), is("theme.css"));
		assertThat(customer.get().getLoginResources().getLoginPageURL(), is("/login"));
		assertThat(customer.get().getJFreeChartPostProcessorClassName(), is("charts.JFree"));
		assertThat(customer.get().getPrimeFacesChartPostProcessorClassName(), is("charts.PrimeFaces"));

		customer.removeTextSearchModuleRole("sales", "SearchOrders")
				.removeTextSearchCustomerRole("SearchAll")
				.removeFlagModuleRole("sales", "FlagOrders")
				.removeFlagCustomerRole("FlagAll")
				.removeSwitchModeModuleRole("sales", "SwitchOrders")
				.removeSwitchModeCustomerRole("SwitchAll")
				.removeInterceptor("example.Interceptor")
				.removeObserver("example.Observer");
		assertTrue(customer.get().getTextSearchRoles().isEmpty());
		assertTrue(customer.get().getFlagRoles().isEmpty());
		assertTrue(customer.get().getSwitchModeRoles().isEmpty());
		assertTrue(customer.get().getInterceptors().isEmpty());
		assertTrue(customer.get().getObservers().isEmpty());

		customer.addTextSearchRole(new FluentCustomerFeatureRole().name("SearchAgain"))
				.addFlagRole(new FluentCustomerFeatureRole().name("FlagAgain"))
				.addSwitchModeRole(new FluentCustomerFeatureRole().name("SwitchAgain"))
				.addInterceptor("example.Interceptor")
				.addObserver("example.Observer")
				.clearTextSearchRoles()
				.clearFlagRoles()
				.clearSwitchModeRoles()
				.clearInterceptors()
				.clearObservers();
		assertTrue(customer.get().getTextSearchRoles().isEmpty());
		assertTrue(customer.get().getFlagRoles().isEmpty());
		assertTrue(customer.get().getSwitchModeRoles().isEmpty());
		assertTrue(customer.get().getInterceptors().isEmpty());
		assertTrue(customer.get().getObservers().isEmpty());
	}

	/** Verifies that {@link FluentCustomer#from(Customer)} copies structured customer metadata. */
	@Test
	@SuppressWarnings("boxing")
	void customerFromCopiesStructuredMetadata() {
		Module sales = mock(Module.class);
		when(sales.getName()).thenReturn("sales");
		when(sales.getFormLabelLayout()).thenReturn(FormLabelLayout.side);
		Module admin = mock(Module.class);
		when(admin.getName()).thenReturn("admin");
		when(admin.getFormLabelLayout()).thenReturn(FormLabelLayout.top);

		CustomerRoleMetaData role = new CustomerRoleMetaData();
		role.setName("Admin");
		role.setDescription("Admin role");
		role.setDocumentation("Docs");
		CustomerModuleRoleMetaData moduleRole = new CustomerModuleRoleMetaData();
		moduleRole.setModuleName("sales");
		moduleRole.setName("Manager");
		role.getRoles().add(moduleRole);

		UIResources uiResources = new UIResources();
		uiResources.setLogoRelativeFileName("logo.svg");
		HTMLResourcesMetaData htmlResources = new HTMLResourcesMetaData();
		htmlResources.setCssRelativeFileName("theme.css");
		LoginResourcesMetaData loginResources = new LoginResourcesMetaData();
		loginResources.setLoginPageURL("/login");
		loginResources.setLoggedOutPageURL("/logout");
		InterceptorMetaDataImpl interceptor = new InterceptorMetaDataImpl();
		interceptor.setClassName("example.Interceptor");
		ObserverMetaDataImpl observer = new ObserverMetaDataImpl();
		observer.setClassName("example.Observer");

		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn("bizhub");
		when(customer.getLanguageTag()).thenReturn("en-AU");
		when(customer.getUiResources()).thenReturn(uiResources);
		when(customer.getHtmlResources()).thenReturn(htmlResources);
		when(customer.getLoginResources()).thenReturn(loginResources);
		when(customer.getDefaultDateConverter()).thenReturn(castConverter(ConverterName.DD_MM_YYYY.getConverter()));
		when(customer.getDefaultTimeConverter()).thenReturn(castConverter(ConverterName.HH24_MI.getConverter()));
		when(customer.getDefaultDateTimeConverter()).thenReturn(castConverter(ConverterName.DD_MM_YYYY_HH24_MI.getConverter()));
		when(customer.getDefaultTimestampConverter()).thenReturn(castConverter(ConverterName.DD_MM_YYYY_HH24_MI_SS.getConverter()));
		when(customer.getModules()).thenReturn(List.of(sales, admin));
		when(customer.getHomeModule()).thenReturn(admin);
		when(customer.getRoles()).thenReturn(List.of(role));
		when(customer.isAllowModuleRoles()).thenReturn(Boolean.TRUE);
		when(customer.getInterceptors()).thenReturn(List.of(interceptor));
		when(customer.getObservers()).thenReturn(List.of(observer));
		when(customer.getJFreeChartPostProcessorClassName()).thenReturn("charts.JFree");
		when(customer.getPrimeFacesChartPostProcessorClassName()).thenReturn("charts.PrimeFaces");

		FluentCustomer fluent = new FluentCustomer().from(customer);
		CustomerMetaData copied = fluent.get();
		assertThat(copied.getName(), is("bizhub"));
		assertThat(copied.getLanguage(), is("en-AU"));
		assertThat(copied.getUiResources().getLogoRelativeFileName(), is("logo.svg"));
		assertThat(copied.getHtmlResources().getCssRelativeFileName(), is("theme.css"));
		assertThat(copied.getLoginResources().getLoggedOutPageURL(), is("/logout"));
		assertThat(copied.getModules().getHomeModule(), is("admin"));
		assertEquals(2, copied.getModules().getModules().size());
		assertTrue(copied.getRoles().isAllowModuleRoles());
		assertEquals(1, copied.getRoles().getRoles().size());
		assertThat(copied.getInterceptors().get(0).getClassName(), is("example.Interceptor"));
		assertThat(copied.getObservers().get(0).getClassName(), is("example.Observer"));
		assertThat(copied.getJFreeChartPostProcessorClassName(), is("charts.JFree"));
		assertThat(copied.getPrimeFacesChartPostProcessorClassName(), is("charts.PrimeFaces"));
	}

	/** Casts a converter instance to the generic type expected by the mocked customer contract. */
	@SuppressWarnings("unchecked")
	private static <T> Converter<T> castConverter(Converter<?> converter) {
		return (Converter<T>) converter;
	}

        @Test
        void textSearchRoleAddFindRemoveClear() {
                FluentCustomer fc = new FluentCustomer();
                FluentCustomerFeatureRole moduleRole = new FluentCustomerFeatureRole().moduleName("sales").name("Search");
                FluentCustomerFeatureRole customerRole = new FluentCustomerFeatureRole().name("GlobalSearch");
                fc.addTextSearchRole(moduleRole).addTextSearchRole(customerRole);
                assertThat(fc.findTextSearchModuleRole("sales", "Search"), notNullValue());
                assertThat(fc.findTextSearchCustomerRole("GlobalSearch"), notNullValue());
                assertThat(fc.findTextSearchModuleRole("sales", "Missing"), nullValue());
                assertThat(fc.findTextSearchCustomerRole("Missing"), nullValue());
                fc.removeTextSearchModuleRole("sales", "Search");
                assertThat(fc.findTextSearchModuleRole("sales", "Search"), nullValue());
                fc.clearTextSearchRoles();
                assertThat(fc.findTextSearchCustomerRole("GlobalSearch"), nullValue());
        }

        @Test
        void flagRoleAddFindRemoveClear() {
                FluentCustomer fc = new FluentCustomer();
                FluentCustomerFeatureRole moduleRole = new FluentCustomerFeatureRole().moduleName("admin").name("Flag");
                FluentCustomerFeatureRole customerRole = new FluentCustomerFeatureRole().name("FlagGlobal");
                fc.addFlagRole(moduleRole).addFlagRole(customerRole);
                assertThat(fc.findFlagModuleRole("admin", "Flag"), notNullValue());
                assertThat(fc.findFlagCustomerRole("FlagGlobal"), notNullValue());
                assertThat(fc.findFlagModuleRole("admin", "Missing"), nullValue());
                assertThat(fc.findFlagCustomerRole("Missing"), nullValue());
                fc.removeFlagModuleRole("admin", "Flag");
                assertThat(fc.findFlagModuleRole("admin", "Flag"), nullValue());
                fc.clearFlagRoles();
                assertThat(fc.findFlagCustomerRole("FlagGlobal"), nullValue());
        }

        @Test
        void switchModeRoleAddFindRemoveClear() {
                FluentCustomer fc = new FluentCustomer();
                FluentCustomerFeatureRole moduleRole = new FluentCustomerFeatureRole().moduleName("admin").name("Switch");
                FluentCustomerFeatureRole customerRole = new FluentCustomerFeatureRole().name("SwitchGlobal");
                fc.addSwitchModeRole(moduleRole).addSwitchModeRole(customerRole);
                assertThat(fc.findSwitchModeModuleRole("admin", "Switch"), notNullValue());
                assertThat(fc.findSwitchModeCustomerRole("SwitchGlobal"), notNullValue());
                assertThat(fc.findSwitchModeModuleRole("admin", "Missing"), nullValue());
                assertThat(fc.findSwitchModeCustomerRole("Missing"), nullValue());
                fc.removeSwitchModeModuleRole("admin", "Switch");
                assertThat(fc.findSwitchModeModuleRole("admin", "Switch"), nullValue());
                fc.clearSwitchModeRoles();
                assertThat(fc.findSwitchModeCustomerRole("SwitchGlobal"), nullValue());
        }

        @Test
        void removeTextSearchCustomerRole() {
                FluentCustomer fc = new FluentCustomer();
                fc.addTextSearchRole(new FluentCustomerFeatureRole().name("CustomerSearch"));
                fc.removeTextSearchCustomerRole("CustomerSearch");
                assertThat(fc.findTextSearchCustomerRole("CustomerSearch"), nullValue());
        }

        @Test
        void removeFlagCustomerRole() {
                FluentCustomer fc = new FluentCustomer();
                fc.addFlagRole(new FluentCustomerFeatureRole().name("CustomerFlag"));
                fc.removeFlagCustomerRole("CustomerFlag");
                assertThat(fc.findFlagCustomerRole("CustomerFlag"), nullValue());
        }

        @Test
        void removeSwitchModeCustomerRole() {
                FluentCustomer fc = new FluentCustomer();
                fc.addSwitchModeRole(new FluentCustomerFeatureRole().name("CustomerSwitch"));
                fc.removeSwitchModeCustomerRole("CustomerSwitch");
                assertThat(fc.findSwitchModeCustomerRole("CustomerSwitch"), nullValue());
        }

	@Test
	void wrappingConstructorUsesProvidedMetaData() {
		org.skyve.impl.metadata.repository.customer.CustomerMetaData meta = new org.skyve.impl.metadata.repository.customer.CustomerMetaData();
		meta.setName("testCustomer");
		FluentCustomer fc = new FluentCustomer(meta);
		assertThat(fc.get(), is(meta));
	}
}
