package org.skyve.impl.metadata.view;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaData;
import org.skyve.impl.metadata.view.InjectBinding;

/**
 * Inserts a bit of script at the location of the tag.
 * 
 * @author mike
 */
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, propOrder = {"script", "bindings"})
@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
public class Inject implements MetaData {
	private static final long serialVersionUID = -4721559398509481425L;

	private String script;
	private List<InjectBinding> bindings = new ArrayList<>();
	
	public String getScript() {
		return script;
	}

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, required = true)
	public void setScript(String script) {
		this.script = UtilImpl.processStringValue(script);
	}

	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "accept")
	public List<InjectBinding> getBindings() {
		return bindings;
	}
}
