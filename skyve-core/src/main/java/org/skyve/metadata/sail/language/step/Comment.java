package org.skyve.metadata.sail.language.step;

import org.skyve.impl.domain.types.jaxb.CDATAAdapter;
import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;
import jakarta.xml.bind.annotation.XmlValue;
import jakarta.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

/**
 * Records a human-readable annotation in the SAIL script that is logged by the executor
 * but does not produce any UI interaction or assertion.
 *
 * <p>The comment text is stored as CDATA in the SAIL XML and passed to
 * {@link org.skyve.metadata.sail.execution.Executor#executeComment} so that execution
 * logs can include descriptive labels for script sections.
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Comment implements Step {

	private String comment;

	/**
	 * Returns the comment.
	 * @return the result
	 */
	public String getComment() {
		return comment;
	}

	/**
	 * Sets the comment.
	 * @param comment the comment
	 */
	@XmlValue
	@XmlJavaTypeAdapter(CDATAAdapter.class)
	public void setComment(String comment) {
		this.comment = UtilImpl.processStringValue(comment);
	}

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeComment(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return "Comment " + comment;
	}
}
