package org.skyve.metadata.sail.language.step;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

/**
 * Make a comment to the reader of the automated script.
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
	public void execute(Executor executor) {
		executor.executeComment(this);
	}
	
	@Override
	public String getIdentifier(AutomationContext context) {
		return "Comment " + comment;
	}
}
