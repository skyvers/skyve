package org.skyve.metadata.customer.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
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
		assertThat(fluentRole.get().getRoles().isEmpty(), is(true));

		FluentCustomerRoles fluentRoles = new FluentCustomerRoles().allowModuleRoles(true).addRole(fluentRole);
		assertThat(fluentRoles.get().isAllowModuleRoles(), is(true));
		assertThat(fluentRoles.findRole("Admin"), is(notNullValue()));
		fluentRoles.removeRole("Admin");
		assertThat(fluentRoles.findRole("Admin"), is(nullValue()));
		fluentRoles.from(List.of(sourceRole), false);
		assertThat(fluentRoles.get().isAllowModuleRoles(), is(false));
		assertThat(fluentRoles.findRole("Admin"), is(notNullValue()));
		fluentRoles.clearRoles();
		assertThat(fluentRoles.get().getRoles().isEmpty(), is(true));
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
		assertThat(modules.get().getModules().size(), is(2));
		modules.removeModule("sales");
		assertThat(modules.get().getHomeModule(), is(nullValue()));
		modules.from(List.of(sales, admin), admin);
		assertThat(modules.get().getHomeModule(), is("admin"));
		assertThat(modules.get().getModules().size(), is(3));
		modules.clearModules();
		assertThat(modules.get().getModules().isEmpty(), is(true));
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
		assertThat(customer.get().getTextSearchRoles().isEmpty(), is(true));
		assertThat(customer.get().getFlagRoles().isEmpty(), is(true));
		assertThat(customer.get().getSwitchModeRoles().isEmpty(), is(true));
		assertThat(customer.get().getInterceptors().isEmpty(), is(true));
		assertThat(customer.get().getObservers().isEmpty(), is(true));

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
		assertThat(customer.get().getTextSearchRoles().isEmpty(), is(true));
		assertThat(customer.get().getFlagRoles().isEmpty(), is(true));
		assertThat(customer.get().getSwitchModeRoles().isEmpty(), is(true));
		assertThat(customer.get().getInterceptors().isEmpty(), is(true));
		assertThat(customer.get().getObservers().isEmpty(), is(true));
	}

	/** Verifies that {@link FluentCustomer#from(Customer)} copies structured customer metadata. */
	@Test
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
		when(customer.isAllowModuleRoles()).thenReturn(true);
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
		assertThat(copied.getModules().getModules().size(), is(2));
		assertThat(copied.getRoles().isAllowModuleRoles(), is(true));
		assertThat(copied.getRoles().getRoles().size(), is(1));
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
}
