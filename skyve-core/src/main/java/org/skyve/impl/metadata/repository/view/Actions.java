package org.skyve.impl.metadata.repository.view;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.repository.view.actions.ActionMetaData;
import org.skyve.impl.metadata.repository.view.actions.AddAction;
import org.skyve.impl.metadata.repository.view.actions.BizExportAction;
import org.skyve.impl.metadata.repository.view.actions.BizImportAction;
import org.skyve.impl.metadata.repository.view.actions.CancelAction;
import org.skyve.impl.metadata.repository.view.actions.CustomAction;
import org.skyve.impl.metadata.repository.view.actions.DefaultsAction;
import org.skyve.impl.metadata.repository.view.actions.DeleteAction;
import org.skyve.impl.metadata.repository.view.actions.DownloadAction;
import org.skyve.impl.metadata.repository.view.actions.NewAction;
import org.skyve.impl.metadata.repository.view.actions.OKAction;
import org.skyve.impl.metadata.repository.view.actions.PrintAction;
import org.skyve.impl.metadata.repository.view.actions.RemoveAction;
import org.skyve.impl.metadata.repository.view.actions.ReportAction;
import org.skyve.impl.metadata.repository.view.actions.SaveAction;
import org.skyve.impl.metadata.repository.view.actions.UploadAction;
import org.skyve.impl.metadata.repository.view.actions.ZoomOutAction;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElementRef;
import jakarta.xml.bind.annotation.XmlElementRefs;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE, name = "actions")
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, 
			name = "actions",
			propOrder = {"widgetId", "actions"})
public class Actions implements Identifiable {
	private static final long serialVersionUID = 7637506523705376564L;

	private String widgetId;
	private List<ActionMetaData> actions = new ArrayList<>();

	@Override
	public String getWidgetId() {
		return widgetId;
	}

	@XmlAttribute(required = false)
	public void setWidgetId(String widgetId) {
		this.widgetId = UtilImpl.processStringValue(widgetId);
	}

	@XmlElementRefs({@XmlElementRef(type = AddAction.class),
						@XmlElementRef(type = BizExportAction.class),
						@XmlElementRef(type = BizImportAction.class),
						@XmlElementRef(type = CancelAction.class),
						@XmlElementRef(type = CustomAction.class),
						@XmlElementRef(type = DefaultsAction.class),
						@XmlElementRef(type = DeleteAction.class),
						@XmlElementRef(type = NewAction.class),
						@XmlElementRef(type = OKAction.class),
						@XmlElementRef(type = RemoveAction.class),
						@XmlElementRef(type = ReportAction.class),
						@XmlElementRef(type = SaveAction.class),
						@XmlElementRef(type = DownloadAction.class),
						@XmlElementRef(type = UploadAction.class),
						@XmlElementRef(type = ZoomOutAction.class),
						@XmlElementRef(type = PrintAction.class)})
	public List<ActionMetaData> getActions() {
		return actions;
	}
}
