package modules.admin.ControlPanel.actions;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import jakarta.annotation.Nonnull;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.ControlPanel.SailExecutor;
import modules.admin.domain.UserProxy;

import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;

/**
 * Executes SAIL automation against the selected runtime context.
 */
@SuppressWarnings("java:S120") // Generated module packages retain document-name casing for compatibility.
public class ExecuteSAIL implements ServerSideAction<ControlPanelExtension> {
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(ExecuteSAIL.class);

	/**
	 * Performs the execute operation.
	 *
	 * @param bean the bean value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext)
	throws Exception {
		executeSAIL(bean);
		return new ServerSideActionResult<>(bean);
	}

	/**
	 * Executes the Control Panel's SAIL with its executor for the Execute button: the generated output, or the
	 * stack trace of what went wrong, goes into the results, and the Results tab is selected.
	 *
	 * @param bean	the Control Panel
	 * @throws ValidationException	if a value the execution needs is not entered, a builder cannot be created or the
	 * 									SAIL user cannot be found
	 */
	static void executeSAIL(ControlPanelExtension bean) {
		bean.setResults(null);
		bean.setTabIndex(null);
		try {
			bean.setResults(generate(bean));
		}
		catch (ValidationException e) {
			throw e;
		}
		catch (Exception e) {
			bean.trapException(e);
		}
		bean.setTabIndex(Integer.valueOf(2));
	}

	/**
	 * Executes the Control Panel's SAIL with its executor, as its SAIL user, and returns what the executor generated.
	 *
	 * <p>Side effects: the persistence's user is the SAIL user during the execution, and is put back afterwards; a mock
	 * Faces context replaces the thread's (a PrimeFaces request's) during it, and the thread's is put back afterwards;
	 * what the execution wrote is rolled back (see {@link GenerateSAIL#discardWrites(org.skyve.persistence.Persistence)}).
	 *
	 * @param bean	the Control Panel
	 * @return	the executor's output: the Java test methods
	 * @throws ValidationException	if a value the execution needs is not entered, a builder cannot be created or the
	 * 									SAIL user cannot be found
	 * @throws ReflectiveOperationException	if the executor cannot be created
	 * @throws org.skyve.metadata.MetaDataException	if the SAIL is invalid, or names what the views do not have
	 */
	static @Nonnull String generate(@Nonnull ControlPanelExtension bean) throws ReflectiveOperationException {
		UserProxy user = bean.getSailUser();
		String baseUrl = bean.getSailBaseUrl();
		SailExecutor executorClass = bean.getSailExecutor();
		String componentBuilderClass = bean.getSailComponentBuilder();
		String layoutBuilderClass = bean.getSailLayoutBuilder();
		String sail = bean.getSail();

		boolean error = false;
		Message message = new Message("Enter or select a value");
		if (user == null) {
			error = true;
			message.addBinding(ControlPanel.sailUserPropertyName);
		}
		if (baseUrl == null) {
			error = true;
			message.addBinding(ControlPanel.sailBaseUrlPropertyName);
		}
		if (executorClass == null) {
			error = true;
			message.addBinding(ControlPanel.sailExecutorPropertyName);
		}
		if (componentBuilderClass == null) {
			error = true;
			message.addBinding(ControlPanel.sailComponentBuilderPropertyName);
		}
		if (layoutBuilderClass == null) {
			error = true;
			message.addBinding(ControlPanel.sailLayoutBuilderPropertyName);
		}
		if (sail == null) {
			error = true;
			message.addBinding(ControlPanel.sailPropertyName);
		}
		if (error) {
			throw new ValidationException(message);
		}

		// Generate in a mock Faces context, even in a PrimeFaces request: in the request's own context the builders
		// would number the components on from the Control Panel page's ids, not as the generated pages number them.
		try (MockFacesContext ignored = MockFacesContext.replacingCurrent()) {
			ClassLoader loader = Thread.currentThread().getContextClassLoader();
			Object componentBuilder = null;
			try {
				componentBuilder = loader.loadClass(componentBuilderClass).getConstructor().newInstance();
			}
			catch (Exception e) {
				LOGGER.error(e.getMessage(), e);
				throw new ValidationException(new Message(ControlPanel.sailComponentBuilderPropertyName,
															"Cannot create component builder: " + e.getMessage()));
			}
			Object layoutBuilder = null;
			try {
				layoutBuilder = loader.loadClass(layoutBuilderClass).getConstructor().newInstance();
			}
			catch (Exception e) {
				LOGGER.error(e.getMessage(), e);
				throw new ValidationException(new Message(ControlPanel.sailLayoutBuilderPropertyName,
															"Cannot create layout builder: " + e.getMessage()));
			}

			AbstractPersistence p = (AbstractPersistence) CORE.getPersistence();
			User currentUser = p.getUser();
			try {
				Automation automation = XMLMetaData.unmarshalSAILString(bean.getSail());

				Repository r = CORE.getRepository();
				@SuppressWarnings("null")
				User u = r.retrieveUser(user.getBizCustomer() + '/' + user.getUserName());
				if (u == null) {
					throw new ValidationException("Cannot find the selected user " + user.getBizCustomer() + '/' + user.getUserName());
				}
				p.setUser(u);

				@SuppressWarnings("null")
				Class<?> type = loader.loadClass(executorClass.toCode());
				Executor executor = (Executor) type.getConstructors()[0].newInstance(componentBuilder, layoutBuilder);
				automation.execute(executor);
				return executor.toString();
			}
			finally {
				// the user first, so that the rest of the request runs as its own user even if discarding fails
				p.setUser(currentUser);
				GenerateSAIL.discardWrites(p);
			}
		}
	}
}
