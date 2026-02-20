package modules.admin.ControlPanel.actions;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Proxy;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import jakarta.inject.Inject;
import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import util.AbstractH2Test;

/**
 * Tests for the SwapCustomer action.
 */
public class SwapCustomerH2Test extends AbstractH2Test {

	private DataBuilder db;
	private ControlPanelExtension controlPanel;

	@Inject
	private SwapCustomer action;

	@BeforeEach
	public void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		controlPanel = db.build(ControlPanel.MODULE_NAME, ControlPanel.DOCUMENT_NAME);
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteWithNullCustomerThrowsValidationException() {
		controlPanel.setTabIndex(5);
		controlPanel.setCustomerNameToSwapTo(null);

		ValidationException e = assertThrows(ValidationException.class, () -> action.execute(controlPanel, null));

		assertThat(controlPanel.getTabIndex(), is(nullValue()));
		assertThat(e.getMessages().size(), is(1));
		assertThat(hasBinding(e.getMessages().get(0).getBindings(), ControlPanel.customerNameToSwapToPropertyName), is(true));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteWithValidCustomerSwapsCustomer() throws Exception {
		UserImpl currentUser = (UserImpl) CORE.getPersistence().getUser();
		String validCustomer = currentUser.getCustomerName();
		String originalResults = controlPanel.getResults();
		currentUser.setCustomerName("invalid-before-swap");
		controlPanel.setTabIndex(3);
		controlPanel.setCustomerNameToSwapTo(validCustomer);

		ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, null);

		assertThat(result, is(notNullValue()));
		assertThat(result.getBean(), is(controlPanel));
		assertThat(controlPanel.getTabIndex(), is(nullValue()));
		assertThat(controlPanel.getResults(), is(originalResults));
		assertThat(currentUser.getCustomerName(), is(validCustomer));
	}

	@SuppressWarnings("boxing")
	@Test
	public void testExecuteWhenSwapFailsTrapsException() throws Exception {
		ProvidedRepository originalRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository failingRepository = (ProvidedRepository) Proxy.newProxyInstance(
				ProvidedRepository.class.getClassLoader(),
				new Class<?>[] { ProvidedRepository.class },
				(proxy, method, args) -> {
					if ("resetMenus".equals(method.getName())) {
						throw new RuntimeException("forced resetMenus failure");
					}
					try {
						return method.invoke(originalRepository, args);
					}
					catch (InvocationTargetException e) {
						throw e.getCause();
					}
				});
		ProvidedRepositoryFactory.set(failingRepository);

		try {
			UserImpl currentUser = (UserImpl) CORE.getPersistence().getUser();
			controlPanel.setTabIndex(9);
			controlPanel.setCustomerNameToSwapTo(currentUser.getCustomerName());

			ServerSideActionResult<ControlPanelExtension> result = action.execute(controlPanel, null);

			assertThat(result, is(notNullValue()));
			assertThat(result.getBean(), is(controlPanel));
			assertThat(controlPanel.getTabIndex(), is(nullValue()));
			assertThat(controlPanel.getResults(), is(notNullValue()));
			assertThat(controlPanel.getResults(), containsString("forced resetMenus failure"));
		}
		finally {
			ProvidedRepositoryFactory.set(originalRepository);
		}
	}

	private static boolean hasBinding(Iterable<String> bindings, String propertyName) {
		for (String binding : bindings) {
			if (binding.contains(propertyName)) {
				return true;
			}
		}
		return false;
	}
}
