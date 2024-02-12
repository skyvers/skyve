package org.skyve.impl.metadata.behaviour;

import java.util.ArrayList;
import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.behaviour.statement.StatementMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.util.ExpressionEvaluator;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlElementWrapper;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, name = "if")
@XmlType(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE,
			name ="if",
			propOrder = {"condition", "thenStatements", "elseStatements"})
public class IfStatement extends StatementMetaData {
	private static final long serialVersionUID = 7250058837673126971L;

	private String condition;

	@XmlElementWrapper(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, name = "then", required = true)
	@XmlElementRefs({@XmlElementRef(type = IfStatement.class),
						@XmlElementRef(type = SetStatement.class)})
	private List<StatementMetaData> thenStatements = new ArrayList<>();

	@XmlElementWrapper(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, name = "else")
	@XmlElementRefs({@XmlElementRef(type = IfStatement.class),
						@XmlElementRef(type = SetStatement.class)})
	private List<StatementMetaData> elseStatements = new ArrayList<>();

	public String getCondition() {
		return condition;
	}
	
	@XmlAttribute(required = true)
	public void setCondition(String condition) {
		this.condition = UtilImpl.processStringValue(condition);
	}

	public List<StatementMetaData> getThenStatements() {
		return thenStatements;
	}

	public List<StatementMetaData> getElseStatements() {
		return elseStatements;
	}

	@Override
	public void execute(Bean bean) {
		if (Boolean.TRUE.equals(ExpressionEvaluator.evaluate(condition, bean))) {
			for (StatementMetaData statement : thenStatements) {
				statement.execute(bean);
			}
		}
		else {
			for (StatementMetaData statement : elseStatements) {
				statement.execute(bean);
			}
		}
	}
}
