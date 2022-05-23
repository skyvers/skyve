package modules.admin.ControlPanel.actions;

import org.skyve.CORE;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.ControlPanel.SailExecutor;
import modules.admin.domain.UserProxy;

public class ExecuteSAIL implements ServerSideAction<ControlPanelExtension> {
	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext)
	throws Exception {
		executeSAIL(bean);
		return new ServerSideActionResult<>(bean);
	}
	
	static void executeSAIL(ControlPanelExtension bean) {
		bean.setResults(null);
		bean.setTabIndex(null);
		
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
		
		ClassLoader loader = Thread.currentThread().getContextClassLoader();
		Object componentBuilder = null;
		try {
			componentBuilder = loader.loadClass(componentBuilderClass).getConstructor().newInstance();
		}
		catch (Exception e) {
			bean.trapException(e);
			throw new ValidationException(new Message(ControlPanel.sailComponentBuilderPropertyName,
														"Cannot create component builder: " + e.getMessage()));
		}
		Object layoutBuilder = null;
		try {
			layoutBuilder = loader.loadClass(layoutBuilderClass).getConstructor().newInstance();
		}
		catch (Exception e) {
			bean.trapException(e);
			throw new ValidationException(new Message(ControlPanel.sailLayoutBuilderPropertyName,
														"Cannot create layout builder: " + e.getMessage()));
		}
		
		AbstractPersistence p = (AbstractPersistence) CORE.getPersistence();
		User currentUser = p.getUser();
		try {
			Automation automation = XMLMetaData.unmarshalSAILString(bean.getSail());

			Repository r = CORE.getRepository();
			@SuppressWarnings("null")
			User u = r.retrieveUser(String.format("%s/%s", user.getBizCustomer(), user.getUserName()));
			p.setUser(u);
			
			@SuppressWarnings("null")
			Class<?> type = loader.loadClass(executorClass.toCode());
			Executor executor = (Executor) type.getConstructors()[0].newInstance(new Object[] {componentBuilder, layoutBuilder});
			automation.execute(executor);
			bean.setResults(executor.toString());
		}
		catch (Exception e) {
			bean.trapException(e);
		}
		finally {
			p.setUser(currentUser);
		}
		bean.setTabIndex(Integer.valueOf(2));
	}
}
