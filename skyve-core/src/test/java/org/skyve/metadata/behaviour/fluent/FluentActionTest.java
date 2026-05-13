package org.skyve.metadata.behaviour.fluent;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.behaviour.IfStatement;
import org.skyve.impl.metadata.behaviour.SetStatement;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.statement.StatementMetaData;

@SuppressWarnings("static-method")
class FluentActionTest {
	@Test
	void setStatementFromCopiesBasePropertiesAndFields() {
		SetStatement source = new SetStatement();
		source.getProperties().put("scope", "request");
		source.setBinding("customer.name");
		source.setExpression("'Skyve'");

		FluentSetStatement fluent = new FluentSetStatement().from(source);

		assertThat(fluent.get().getProperties().get("scope"), is("request"));
		assertThat(fluent.get().getBinding(), is("customer.name"));
		assertThat(fluent.get().getExpression(), is("'Skyve'"));
	}

	@Test
	void ifStatementFromCopiesConditionAndNestedStatements() {
		IfStatement source = new IfStatement();
		source.getProperties().put("branch", "main");
		source.setCondition("bean.active");
		source.getThenStatements().add(new SetStatement());
		source.getElseStatements().add(new SetStatement());

		FluentIfStatement fluent = new FluentIfStatement().from(source);

		assertThat(fluent.get().getProperties().get("branch"), is("main"));
		assertThat(fluent.get().getCondition(), is("bean.active"));
		assertThat(fluent.get().getThenStatements().size(), is(1));
		assertThat(fluent.get().getElseStatements().size(), is(1));
	}

	@Test
	void actionManagesStatementsAndCopiesFromSource() {
		FluentSetStatement first = new FluentSetStatement().binding("one").expression("1");
		FluentSetStatement second = new FluentSetStatement().binding("two").expression("2");
		FluentAction fluent = new FluentAction().name("save").documentation("Save action")
				.addStatement(first)
				.addStatement(0, second);

		assertThat(fluent.get().getStatements().size(), is(2));
		assertThat(((SetStatement) fluent.get().getStatements().get(0)).getBinding(), is("two"));

		ActionMetaData source = new ActionMetaData();
		source.setName("copied");
		source.setDocumentation("Copied action");
		source.getStatements().add(first.get());
		source.getStatements().add(new FluentIfStatement().condition("bean.active").get());
		FluentAction copied = new FluentAction().from(source);
		assertThat(copied.get().getName(), is("copied"));
		assertThat(copied.get().getDocumentation(), is("Copied action"));
		assertThat(copied.get().getStatements().size(), is(2));
		assertThat(copied.get().getStatements().get(1), is(instanceOf(IfStatement.class)));

		fluent.clearStatements();
		assertThat(fluent.get().getStatements().isEmpty(), is(true));
	}

	@Test
	void ifStatementManagesThenAndElseBranches() {
		FluentSetStatement thenStatement = new FluentSetStatement().binding("then").expression("1");
		FluentSetStatement elseStatement = new FluentSetStatement().binding("else").expression("0");
		FluentIfStatement fluent = new FluentIfStatement().condition("bean.active")
				.addThenStatement(thenStatement)
				.addElseStatement(elseStatement)
				.addThenStatement(0, new FluentSetStatement().binding("first").expression("2"))
				.addElseStatement(0, new FluentSetStatement().binding("fallback").expression("3"));

		assertThat(fluent.get().getThenStatements().size(), is(2));
		assertThat(((SetStatement) fluent.get().getThenStatements().get(0)).getBinding(), is("first"));
		assertThat(fluent.get().getElseStatements().size(), is(2));
		assertThat(((SetStatement) fluent.get().getElseStatements().get(0)).getBinding(), is("fallback"));

		fluent.clearThenStatements().clearElseStatements();
		assertThat(fluent.get().getThenStatements().isEmpty(), is(true));
		assertThat(fluent.get().getElseStatements().isEmpty(), is(true));
	}

	@Test
	void statementFactoryReturnsExpectedFluentTypes() {
		FluentStatement<?> fromSet = FluentStatement.from(new SetStatement());
		FluentStatement<?> fromIf = FluentStatement.from(new IfStatement());

		assertThat(fromSet, is(instanceOf(FluentSetStatement.class)));
		assertThat(fromIf, is(instanceOf(FluentIfStatement.class)));
	}

	@Test
	void statementFactoryRejectsUnsupportedStatements() {
		StatementMetaData unknown = new StatementMetaData() {
			private static final long serialVersionUID = 1L;

			@Override
			public void execute(Bean bean) {
				// no-op
			}
		};

		IllegalStateException ex = assertThrows(IllegalStateException.class, () -> FluentStatement.from(unknown));
		assertThat(ex.getMessage(), is(notNullValue()));
	}
}
