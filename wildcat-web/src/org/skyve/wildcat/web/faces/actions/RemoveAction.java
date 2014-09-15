package org.skyve.wildcat.web.faces.actions;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.util.Util;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.util.UtilImpl;
import org.skyve.wildcat.web.faces.FacesAction;
import org.skyve.wildcat.web.faces.beans.FacesView;

public class RemoveAction extends FacesAction<Void> {
	private FacesView<? extends Bean> facesView;
	private String collectionName;
	private String elementBizId;
	public RemoveAction(FacesView<? extends Bean> facesView,
							String collectionName,
							String elementBizId) {
		this.facesView = facesView;
		this.collectionName = collectionName;
		this.elementBizId = elementBizId;
	}

	@Override
	public Void callback() throws Exception {
		if (UtilImpl.FACES_TRACE) Util.LOGGER.info("RemoveAction - collectionName=" + collectionName + " : elementBizId=" + elementBizId);

		Bean bean = facesView.getBean();
		String viewBinding = facesView.getViewBinding();
		if ((collectionName != null) && (elementBizId != null)) { // inline remove
			Bean beanToRemove = ActionUtil.getTargetBeanForViewAndCollectionBinding(facesView, collectionName, elementBizId);

			StringBuilder collectionBinding = new StringBuilder(32);
			if (viewBinding != null) {
				collectionBinding.append(viewBinding).append('.');
			}
			collectionBinding.append(collectionName);
			@SuppressWarnings("unchecked")
			List<Bean> list = (List<Bean>) BindUtil.get(bean, collectionBinding.toString());

			list.remove(beanToRemove);
			if (beanToRemove instanceof ChildBean<?>) {
				((ChildBean<?>) beanToRemove).setParent(null);
			}
		}
		else { // Remove on zoomed view
			int lastCollectionindex = viewBinding.lastIndexOf("ElementById(");
			@SuppressWarnings("unchecked")
			List<Bean> list = (List<Bean>) BindUtil.get(bean, viewBinding.substring(0, lastCollectionindex));
			Bean beanToRemove = (Bean) BindUtil.get(bean, viewBinding);
			list.remove(beanToRemove);
			if (beanToRemove instanceof ChildBean<?>) {
				((ChildBean<?>) beanToRemove).setParent(null);
			}
			
			ZoomOutAction.zoomOut(facesView);
		}

	    return null;
	}
}
