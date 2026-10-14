package org.skyve.impl.metadata.repository.behaviour;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.skyve.domain.Bean;
import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.metadata.behaviour.IfStatement;
import org.skyve.impl.metadata.behaviour.InvokeStatement;
import org.skyve.impl.metadata.behaviour.InvokeStaticStatement;
import org.skyve.impl.metadata.behaviour.MethodArgument;
import org.skyve.impl.metadata.behaviour.SetStatement;
import org.skyve.impl.metadata.repository.ConvertibleMetaData;
import org.skyve.impl.metadata.repository.PropertyMapAdapter;
import org.skyve.impl.metadata.repository.behaviour.statement.StatementMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.ReloadableMetaData;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * JAXB root element for an action descriptor ({@code *.xml} in the module actions
 * package), converted to a runtime action metadata object.
 *
 * <p>An action descriptor names the action, provides documentation, and holds an
 * ordered list of {@link StatementMetaData} behaviour statements.  The
 * {@link ConvertibleMetaData} interface drives conversion to the runtime action
 * representation during repository bootstrap.
 *
 * <p>Threading: not thread-safe.  Instances are populated during JAXB unmarshalling
 * and are read-only once placed in the repository cache.
 *
 * @see BizletMetaData
 * @see StatementMetaData
 */
@XmlRootElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, name = "action")
@XmlType(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE, 
			name = "action",
			propOrder = {"name", "documentation", "statements", "properties"})
public class ActionMetaData implements NamedMetaData, ConvertibleMetaData<ActionMetaData>, ReloadableMetaData, DecoratedMetaData {
	private static final long serialVersionUID = 226463757653299558L;

	private String name;
	private String documentation;

	@XmlElementRefs({@XmlElementRef(type = IfStatement.class),
						@XmlElementRef(type = SetStatement.class),
						@XmlElementRef(type = InvokeStatement.class),
						@XmlElementRef(type = InvokeStaticStatement.class)})
	private List<StatementMetaData> statements = new ArrayList<>();

	private long lastModifiedMillis = Long.MAX_VALUE;
	private long lastCheckedMillis = System.currentTimeMillis();
	
	@XmlElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE)
	@XmlJavaTypeAdapter(PropertyMapAdapter.class)
	private Map<String, String> properties = new LinkedHashMap<>();

	/**
	 * Returns the logical action name declared in metadata.
	 *
	 * @return action name, or {@code null} when unspecified
	 */
	@Override
	public @Nonnull String getName() {
		return name;
	}

	/**
	 * Sets the logical action name declared in metadata.
	 *
	 * @param name action name, trimmed to {@code null} when blank
	 */
	@XmlAttribute(required = true)
	public void setName(@Nonnull String name) {
		this.name = UtilImpl.processStringValue(name);
	}

	/**
	 * Returns action-level documentation sourced from metadata.
	 *
	 * @return documentation text, or {@code null} when unspecified
	 */
	public @Nullable String getDocumentation() {
		return documentation;
	}
	
	/**
	 * Sets action-level documentation sourced from metadata.
	 *
	 * @param documentation documentation text, trimmed to {@code null} when blank
	 */
	@XmlElement(namespace = XMLMetaData.BEHAVIOUR_NAMESPACE)
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setDocumentation(@Nullable String documentation) {
		this.documentation = UtilImpl.processStringValue(documentation);
	}

	/**
	 * Returns the ordered behaviour statements configured for this action.
	 *
	 * @return mutable statement list in declaration order
	 */
	public @Nonnull List<StatementMetaData> getStatements() {
		return statements;
	}

	/**
	 * Returns the source timestamp of the metadata resource backing this action.
	 *
	 * @return last-modified timestamp in milliseconds since epoch
	 */
	@Override
	public long getLastModifiedMillis() {
		return lastModifiedMillis;
	}

	/**
	 * Sets the source timestamp of the metadata resource backing this action.
	 *
	 * @param lastModifiedMillis last-modified timestamp in milliseconds since epoch
	 */
	@XmlTransient
	public void setLastModifiedMillis(long lastModifiedMillis) {
		this.lastModifiedMillis = lastModifiedMillis;
	}
	
	/**
	 * Returns the timestamp of the most recent reload check.
	 *
	 * @return last-checked timestamp in milliseconds since epoch
	 */
	@Override
	public long getLastCheckedMillis() {
		return lastCheckedMillis;
	}

	/**
	 * Sets the timestamp of the most recent reload check.
	 *
	 * @param lastCheckedMillis last-checked timestamp in milliseconds since epoch
	 */
	@Override
	@XmlTransient
	public void setLastCheckedMillis(long lastCheckedMillis) {
		this.lastCheckedMillis = lastCheckedMillis;
	}

	/**
	 * Validates and finalises this action descriptor after unmarshalling.
	 *
	 * @param metaDataName metadata path used in validation error messages
	 * @return this action metadata instance
	 * @throws MetaDataException if the action name is missing
	 */
	@Override
	public @Nonnull ActionMetaData convert(@Nonnull String metaDataName) {
		String theName = getName();
		if (theName == null) {
			throw new MetaDataException(metaDataName + " : The action [name] is required for action " + metaDataName);
		}

		// validate statements recursively
		List<StatementMetaData> statementsToValidate = new ArrayList<>(statements);
		for (int i = 0; i < statementsToValidate.size(); i++) {
			StatementMetaData statement = statementsToValidate.get(i);
			if (statement instanceof IfStatement ifStatement) {
				if (ifStatement.getCondition() == null) {
					throw new MetaDataException(metaDataName + " : The if [condition] is required for action " + theName);
				}
				statementsToValidate.addAll(ifStatement.getThenStatements());
				statementsToValidate.addAll(ifStatement.getElseStatements());
			}
			else if (statement instanceof SetStatement setStatement) {
				if (setStatement.getBinding() == null) {
					throw new MetaDataException(metaDataName + " : The set [binding] is required for action " + theName);
				}
				if (setStatement.getExpression() == null) {
					throw new MetaDataException(metaDataName + " : The set [expression] is required for action " + theName);
				}
			}
			else if (statement instanceof InvokeStatement invokeStatement) {
				if ((invokeStatement instanceof InvokeStaticStatement invokeStaticStatement) &&
						(invokeStaticStatement.getClassName() == null)) {
					throw new MetaDataException(metaDataName + " : The invokeStatic [className] is required for action " + theName);
				}
				if (invokeStatement.getMethodName() == null) {
					throw new MetaDataException(metaDataName + " : The invoke [method] is required for action " + theName);
				}
				for (MethodArgument argument : invokeStatement.getArguments()) {
					if (argument.getTypeName() == null) {
						throw new MetaDataException(metaDataName + " : The argument [typeName] is required for action " + theName);
					}
					if (argument.getExpression() == null) {
						throw new MetaDataException(metaDataName + " : The argument [expression] is required for action " + theName);
					}
				}
			}
		}
		
		return this;
	}
	
	/**
	 * Returns decorator properties associated with this action metadata.
	 *
	 * @return mutable action property map
	 */
	@Override
	public @Nonnull Map<String, String> getProperties() {
		return properties;
	}
	
	/**
	 * Executes all configured statements against the supplied bean.
	 *
	 * <p>Statements run in declaration order.
	 *
	 * @param bean the document instance bound to this action execution
	 */
	public void execute(@Nonnull Bean bean) {
		statements.forEach(s -> s.execute(bean));
	}
}
