package org.skyve.metadata.sail.language.step;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.ExecutionOptions;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * A non-executable SAIL step that adds a human-readable comment to the script.
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Comment implements Step {

	private String comment;

	public String getComment() {
		return comment;
	}

	@XmlValue
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setComment(String comment) {
		this.comment = UtilImpl.processStringValue(comment);
	}

	@Override
	public void execute(Executor executor, ExecutionOptions options) {
		executor.executeComment(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return "Comment " + comment;
	}
}
