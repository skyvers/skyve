package org.skyve.impl.metadata.repository.view.access;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.SerializableMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlType;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, name = "accesses")
public class ViewUserAccessesMetaData implements SerializableMetaData {
	private static final long serialVersionUID = 802609954407129386L;
	
	private List<ViewUserAccessMetaData> accesses = new ArrayList<>();
	private boolean generate = true;
	
	@XmlElementRefs({@XmlElementRef(type = ViewSingularUserAccessMetaData.class), 
						@XmlElementRef(type = ViewDocumentAggregateUserAccessMetaData.class),
						@XmlElementRef(type = ViewQueryAggregateUserAccessMetaData.class),
						@XmlElementRef(type = ViewModelAggregateUserAccessMetaData.class),
						@XmlElementRef(type = ViewPreviousCompleteUserAccessMetaData.class),
						@XmlElementRef(type = ViewReportUserAccessMetaData.class),
						@XmlElementRef(type = ViewDynamicImageUserAccessMetaData.class),
						@XmlElementRef(type = ViewContentUserAccessMetaData.class)})
	public List<ViewUserAccessMetaData> getAccesses() {
		return accesses;
	}
	
	public boolean isGenerate() {
		return generate;
	}

	@XmlAttribute(required = true)
	public void setGenerate(boolean generate) {
		this.generate = generate;
	}
}
