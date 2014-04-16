// NClass - Free class diagram editor
// Copyright (C) 2006-2007 Balazs Tihanyi
// 
// This program is free software; you can redistribute it and/or modify it under 
// the terms of the GNU General Public License as published by the Free Software 
// Foundation; either version 3 of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful, but WITHOUT 
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with 
// this program; if not, write to the Free Software Foundation, Inc., 
// 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

using System;
using System.Drawing;
using System.Collections.Generic;

namespace NClass.GUI.Diagram
{
	public interface IDiagram
	{
		// Properties
		Project Project { get; }

		float Zoom { get; }

		int ZoomPercentage { get; set; }

		DiagramElement FirstSelectedElement { get; }

		int ElementsCount { get; }

		bool IsEmpty { get; }

		int SelectedElementCount { get; }

		bool HasSelectedElement { get; }

		bool SingleSelection { get; }

		bool MultipleSelection { get; }


		// Methods
        IEnumerable<DiagramElement> GetElements();

        IEnumerable<DiagramElement> GetSelectedElements();

        void ChangeZoom(bool enlarge);

		void ChangeZoom(bool enlarge, PointF zoomingCenter);

		void ChangeZoom(float zoom);

		void ChangeZoom(float zoomValue, PointF zoomingCenter);

		void AutoZoom(bool selectedOnly);

		void SelectAll();

		void DeleteSelectedElements();

		void AlignLeft();

		void AlignRight();

		void AlignTop();

		void AlignBottom();

		void AlignHorizontal();

		void AlignVertical();

		void AdjustToSameWidth();

		void AdjustToSameHeight();

		void AdjustToSameSize();

		void AutoWidthOfSelectedShapes();

		void AutoHeightOfSelectedShapes();

		void CollapseAll();

		void CollapseAll(bool selectedOnly);

		void ExpandAll();
		
		void ExpandAll(bool selectedOnly);

		void Print();

		void CopyAsImage();
		
		void SaveAsImage();

		void SaveAsImage(bool selectedOnly);
	}
}
