package util.sail;

import org.openqa.selenium.JavascriptExecutor;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.sail.execution.WebDriverExecutor;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.sail.language.step.Comment;
import org.skyve.metadata.sail.language.step.Execute;
import org.skyve.metadata.sail.language.step.Pause;
import org.skyve.metadata.sail.language.step.TestFailure;
import org.skyve.metadata.sail.language.step.TestSuccess;
import org.skyve.metadata.sail.language.step.context.ClearContext;
import org.skyve.metadata.sail.language.step.context.PopContext;
import org.skyve.metadata.sail.language.step.context.PushEditContext;
import org.skyve.metadata.sail.language.step.context.PushListContext;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateEdit;
import org.skyve.metadata.sail.language.step.interaction.navigation.NavigateList;
import org.skyve.metadata.sail.language.step.interaction.session.Login;
import org.skyve.metadata.sail.language.step.interaction.session.Logout;

/**
 * Abstract base executor that interprets and executes web automation steps
 * using a generic {@link Selenide} implementation and automation context.
 * <p>
 * Implementations extend this class to bind specific automation contexts and
 * Selenide subclasses for UI testing across different web frameworks.
 * 
 * @param <AC> automation context type
 * @param <S> concrete Selenide implementation
 * @author simeonsolomou
 */
public abstract class InterpretedWebDriverExecutor<
	AC extends AutomationContext<?, ?>,
	S extends Selenide<?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?>> extends WebDriverExecutor<AC> {

	protected S selenide;

	@Override
	public void executeClearContext(ClearContext clear) {
		clear();
	}

	@Override
	public void executePopContext(PopContext pop) {
		pop();
	}

	@Override
	public void executeLogin(Login login) {
		String customer = login.getCustomer();
		String user = login.getUser();

		if (customer == null) {
			selenide.trace("Login as " + user);
		} else {
			selenide.trace("Login as " + customer + "/" + user);
		}
		selenide.login(customer, user, login.getPassword());
	}

	@Override
	public void executeLogout(Logout logout) {
		selenide.trace("Logout");
		selenide.logout();
	}

	@Override
	public void executeNavigateList(NavigateList list) {
		String moduleName = list.getModuleName();
		String documentName = list.getDocumentName();
		String queryName = list.getQueryName();
		String modelName = list.getModelName();

		PushListContext push = new PushListContext();
		push.setModuleName(moduleName);
		push.setDocumentName(documentName);
		push.setQueryName(queryName);
		push.setModelName(modelName);
		executePushListContext(push);

		if (queryName != null) {
			selenide.trace(String.format("List for query [%s.%s]", moduleName, queryName));
			selenide.get(String.format("?a=l&m=%s&q=%s", moduleName, queryName));
		} else if (documentName != null) {
			if (modelName != null) {
				selenide.trace(String.format("List for model [%s.%s.%s]", moduleName, documentName, modelName));
				selenide.get(String.format("?a=l&m=%s&d=%s&q=%s", moduleName, documentName, modelName));
			} else {
				selenide.trace(String.format("List for default query of [%s.%s]", moduleName, documentName));
				selenide.get(String.format("?a=l&m=%s&q=%s", moduleName, documentName));
			}
		}
	}

	@Override
	public void executeNavigateEdit(NavigateEdit edit) {
		String moduleName = edit.getModuleName();
		String documentName = edit.getDocumentName();

		PushEditContext push = new PushEditContext();
		push.setModuleName(moduleName);
		push.setDocumentName(documentName);
		executePushEditContext(push);

		String bizId = edit.getBizId();
		if (bizId == null) {
			selenide.trace(String.format("Edit new document [%s.%s] instance", moduleName, documentName));
			selenide.get(String.format("?a=e&m=%s&d=%s", moduleName, documentName));
		} else {
			selenide.trace(String.format("Edit document [%s.%s] instance with bizId %s", moduleName, documentName, bizId));
			selenide.get(String.format("?a=e&m=%s&d=%s&i=%s", moduleName, documentName, bizId));
		}
	}

	@Override
	public void executeTestSuccess(TestSuccess success) {
		TestStrategy strategy = getTestStrategy();

		if (TestStrategy.Assert == strategy) {
			selenide.trace("Asserting Success");
			selenide.assertSuccess();
		} else if (TestStrategy.Verify == strategy) {
			selenide.trace("Verifying Success");
			selenide.verifySuccess();
		}
	}

	@Override
	public void executeTestFailure(TestFailure failure) {
		TestStrategy strategy = getTestStrategy();

		if (TestStrategy.None != strategy) {
			String message = failure.getMessage();

			if (message == null) {
				comment("Test Failure");
			} else {
				selenide.trace(String.format("Test Failure with message '%s'", message));
			}

			if (TestStrategy.Verify == strategy) {
				if (message == null) {
					selenide.verifyFailure();
				} else {
					selenide.verifyFailure(message);
				}
			} else {
				if (message == null) {
					selenide.assertFailure();
				} else {
					selenide.assertFailure(message);
				}
			}
		}
	}

	@Override
	public void executeComment(Comment comment) {
		selenide.trace(comment.getComment());
	}

	@Override
	public void executeExecute(Execute execute) {
		JavascriptExecutor js = (JavascriptExecutor) selenide.driver;
		js.executeScript("javascript:" + execute.getScript());
	}

	@Override
	public void executePause(Pause pause) {
		selenide.pause(pause.getMillis());
	}
}
