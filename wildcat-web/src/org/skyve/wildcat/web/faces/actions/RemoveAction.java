package org.skyve.wildcat.web.faces.actions;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.web.faces.FacesAction;
import org.skyve.wildcat.web.faces.beans.FacesView;

public class RemoveAction extends FacesAction<Void> {
	private FacesView<? extends Bean> facesView;
	public RemoveAction(FacesView<? extends Bean> facesView) {
		this.facesView = facesView;
	}

	@Override
	public Void callback() throws Exception {
		Bean bean = facesView.getBean();
		String viewBinding = facesView.getViewBinding();
		int lastCollectionindex = viewBinding.lastIndexOf("ElementById(");
		@SuppressWarnings("unchecked")
		List<Bean> list = (List<Bean>) BindUtil.get(bean, viewBinding.substring(0, lastCollectionindex));
		Bean beanToRemove = (Bean) BindUtil.get(bean, viewBinding);
		list.remove(beanToRemove);
		
		ZoomOutAction.zoomOut(facesView);

		return null;
	}
}
