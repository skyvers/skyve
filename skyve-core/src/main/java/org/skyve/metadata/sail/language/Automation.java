package org.skyve.metadata.sail.language;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;

@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"before", "interactions", "after"})
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Automation implements Executable {
	private Procedure before;
	private List<Interaction> interactions = new ArrayList<>();
	private Procedure after;
	
	public Procedure getBefore() {
		return before;
	}

	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "before")
	public void setBefore(Procedure before) {
		this.before = before;
	}

	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "interaction", required = true)
	public List<Interaction> getInteractions() {
		return interactions;
	}

	public Procedure getAfter() {
		return after;
	}

	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "after")
	public void setAfter(Procedure after) {
		this.after = after;
	}
	
	@Override
	public void execute(Executor executor) {
		executor.execute(this);
	}
}
