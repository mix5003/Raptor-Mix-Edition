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
using System.Reflection;
using System.Collections.Generic;
using System.Xml;
using System.IO;
using NClass.Translations;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace NClass.Core
{
    public class ProjectCore
	{
		string projectFile = null;
        List<IEntity> entities = new List<IEntity>();
        List<Relation> relations = new List<Relation>();
        Language language;
        bool contentModified = false;
        bool silentWork = false;
        public static RAPTORUpdater raptorUpdater;

        public event EventHandler Cleared;
        public event EventHandler ContentChanged;
        public event EventHandler LanguageChanged;
        public event EventHandler FileStateChanged;
        public event EntityEventHandler EntityAdded;
        public event EntityEventHandler EntityRemoved;
        public event RelationEventHandler RelationAdded;
        public event RelationEventHandler RelationRemoved;
        public event SerializeEventHandler Serializing;
        public event SerializeEventHandler Deserializing;

		/// <exception cref="ArgumentNullException">
		/// <paramref name="language"/> is null.
		/// </exception>
		public ProjectCore(Language language)
		{
			if (language == null)
				throw new ArgumentNullException("language");

			this.language = language;
		}

		/// <exception cref="IOException">
		/// Could not load the project.
		/// </exception>
		/// <exception cref="InvalidDataException">
		/// The save file is corrupt and could not load.
		/// </exception>
		/// <exception cref="ArgumentException">
		/// <paramref name="fileName"/> is empty string.
		/// </exception>
		public ProjectCore(string fileName)
		{
			Load(fileName);
		}

		public IEnumerable<IEntity> Entities
		{
			get { return entities; }
		}

		public IEnumerable<Relation> Relations
		{
			get { return relations; }
		}

		public string ProjectFile
		{
			get
			{
				return projectFile;
			}
			private set
			{
				if (projectFile != value) {
					projectFile = value;
					OnFileStateChanged(EventArgs.Empty);
				}
			}
		}

		public bool IsUntitled
		{
			get { return string.IsNullOrEmpty(ProjectFile); }
		}

		public string ProjectFileName
		{
			get
			{
				if (IsUntitled)
					return Strings.GetString("untitled");
				else
					return Path.GetFileName(ProjectFile);
			}
		}

		public string ProjectFileNameWithoutExtension
		{
			get
			{
				if (IsUntitled)
					return Strings.GetString("untitled");
				else
					return Path.GetFileNameWithoutExtension(ProjectFile);
			}
		}

		public string ProjectDirectory
		{
			get
			{
				if (IsUntitled)
					return Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
				else
					return Path.GetDirectoryName(ProjectFile);
			}
		}
		
		public Language Language
		{
			get
			{
				return language;
			}
			set
			{
				if (language != value) {
					language = value;
					OnLanguageChanged(EventArgs.Empty);
				}
			}
		}

		public bool SilentWork
		{
			get
			{
				return silentWork;
			}
			set
			{
				silentWork = value;
				if (!silentWork && contentModified)
					OnContentChanged(EventArgs.Empty);
			}
		}

		public bool IsEmpty
		{
			get
			{
				return (entities.Count == 0 && relations.Count == 0);
			}
		}

		protected void ContentModified()
		{
			if (!SilentWork)
				OnContentChanged(EventArgs.Empty);
			else
				contentModified = true;
		}

		public void AddEntity(IEntity entity)
		{
			if (entity != null) {
				entities.Add(entity);
				entity.Changed += delegate { ContentModified(); };
				OnEntityAdded(new EntityEventArgs(entity));
			}
		}

		public ClassType AddClass()
		{
			ClassType newClass = Language.CreateClass();

			AddEntity(newClass);
            newClass.raptorTab = raptorUpdater.createClass(newClass.Name, newClass);
			return newClass;
		}

		/// <exception cref="InvalidOperationException">
		/// The language does not support structs.
		/// </exception>
		public StructureType AddStructure()
		{
			StructureType structure = Language.CreateStruct();

			AddEntity(structure);
			return structure;
		}

		public InterfaceType AddInterface()
		{
			InterfaceType newInterface = Language.CreateInterface();

			AddEntity(newInterface);
			return newInterface;
		}

		public EnumType AddEnum()
		{
			EnumType newEnum = Language.CreateEnum();

			AddEntity(newEnum);
			return newEnum;
		}

		/// <exception cref="InvalidOperationException">
		/// The language does not support delegates.
		/// </exception>
		public DelegateType AddDelegate()
		{
			DelegateType newDelegate = Language.CreateDelegate();

			AddEntity(newDelegate);
			return newDelegate;
		}

		public Comment AddComment()
		{
			Comment comment = new Comment();

			AddEntity(comment);
			return comment;
		}

		/// <exception cref="RelationException">
		/// Cannot add expired relationship.
		/// </exception>
		private void AddRelation(Relation relation)
		{
			relation.Changed += delegate { ContentModified(); };
			relations.Add(relation);
			OnRelationAdded(new RelationEventArgs(relation));
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="first"/> is null.-or-
		/// <paramref name="second"/> is null.
		/// </exception>
		public AssociationRelation AddAssociationRelation(TypeBase first, TypeBase second)
		{
			AssociationRelation association = new AssociationRelation(first, second);

			AddRelation(association);
			return association;
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="first"/> is null.-or-
		/// <paramref name="second"/> is null.
		/// </exception>
		public AssociationRelation AddCompositionRelation(TypeBase first, TypeBase second)
		{
			AssociationRelation association = new AssociationRelation(first, second);
			association.IsComposition = true;

			AddRelation(association);
			return association;
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="first"/> is null.-or-
		/// <paramref name="second"/> is null.
		/// </exception>
		public AssociationRelation AddAggregationRelation(TypeBase first, TypeBase second)
		{
			AssociationRelation association = new AssociationRelation(first, second);
			association.IsAggregation = true;

			AddRelation(association);
			return association;
		}

		/// <exception cref="RelationException">
		/// Cannot create relationship between the two types.
		/// </exception>
		/// <exception cref="ArgumentNullException">
		/// <paramref name="derivedType"/> is null.-or-
		/// <paramref name="baseType"/> is null.
		/// </exception>
		public GeneralizationRelation AddGeneralizationRelation(
			CompositeType derivedType, CompositeType baseType)
		{
			GeneralizationRelation generalization =  
				new GeneralizationRelation(derivedType, baseType);

			AddRelation(generalization);
			return generalization;
		}

		/// <exception cref="RelationException">
		/// Cannot create relationship between the two types.
		/// </exception>
		/// <exception cref="ArgumentNullException">
		/// <paramref name="derivedType"/> is null.-or-
		/// <paramref name="baseType"/> is null.
		/// </exception>
		public RealizationRelation AddRealizationRelation(
			TypeBase implementer, InterfaceType baseType)
		{
			RealizationRelation realization = new RealizationRelation(implementer, baseType);

			AddRelation(realization);
			return realization;
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="first"/> is null.-or-
		/// <paramref name="second"/> is null.
		/// </exception>
		public DependencyRelation AddDependencyRelation(TypeBase first, TypeBase second)
		{
			DependencyRelation dependency = new DependencyRelation(first, second);

			AddRelation(dependency);
			return dependency;
		}

		/// <exception cref="RelationException">
		/// Cannot create relationship between the two types.
		/// </exception>
		/// <exception cref="ArgumentNullException">
		/// <paramref name="parentType"/> is null.-or-
		/// <paramref name="innerType"/> is null.
		/// </exception>
		public NestingRelation AddNestingRelation(CompositeType parentType, TypeBase innerType)
		{
			NestingRelation nesting = new NestingRelation(parentType, innerType);

			AddRelation(nesting);
			return nesting;
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="comment"/> is null.-or-
		/// <paramref name="entity"/> is null.
		/// </exception>
		public CommentRelation AddCommentRelation(Comment comment, IEntity entity)
		{
			CommentRelation commentRelation = new CommentRelation(comment, entity);

			AddRelation(commentRelation);
			return commentRelation;
		}

		public void RemoveEntity(IEntity entity)
		{
			entities.Remove(entity);
			RemoveRelations(entity);
            if (entity is ClassType)
            {
                raptorUpdater.deleteClass((entity as ClassType).raptorTab);
            }
			OnEntityRemoved(new EntityEventArgs(entity));
		}

		private void RemoveRelations(IEntity entity)
		{
			for (int i = 0; i < relations.Count; i++) {
				Relation relation = relations[i];
				if (relation.First == entity || relation.Second == entity) {
					relation.Detach();
					relations.RemoveAt(i--);
					OnRelationRemoved(new RelationEventArgs(relation));
				}
			}
		}

		public void RemoveRelation(Relation relation)
		{
			if (relations.Contains(relation)) {
				relation.Detach();
				relations.Remove(relation);
				OnRelationRemoved(new RelationEventArgs(relation));
			}
		}

		public virtual void ClearProject()
		{
			contentModified = false;
			entities.Clear();
			relations.Clear();
			projectFile = null;

			OnCleared(EventArgs.Empty);
		}

		public virtual bool NewProject()
		{
			ClearProject();
			return true;
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="language"/> is null.
		/// </exception>
		public bool NewProject(Language language)
		{
			if (language == null)
				throw new ArgumentNullException("language");

			if (NewProject()) {
				Language = language;
				return true;
			}
			else {
				return false;
			}
		}

		/// <exception cref="IOException">
		/// Could not load the project.
		/// </exception>
		/// <exception cref="InvalidDataException">
		/// The save file is corrupt and could not load.
		/// </exception>
		/// <exception cref="ArgumentException">
		/// <paramref name="fileName"/> is empty string.
		/// </exception>
		public virtual void Load(string fileName)
		{
			if (string.IsNullOrEmpty(fileName))
				throw new ArgumentException(Strings.GetString("error_blank_filename"), "fileName");

			if (!File.Exists(fileName))
				throw new FileNotFoundException(Strings.GetString("error_file_not_found"));

			XmlDocument document = new XmlDocument();
			try {
				document.Load(fileName);
			}
			catch (Exception ex) {
				throw new IOException(Strings.GetString("error_could_not_load_file"), ex);
			}

			try {
				XmlElement root = document["ClassProject"];
				if (root == null)
					throw new InvalidDataException(Strings.GetString("error_corrupt_savefile"));

				SilentWork = true;
				if (NewProject()) {
					try {
						Deserialize(root);
					}
					catch (Exception ex) {
						ClearProject();
						throw new InvalidDataException(Strings.GetString("error_corrupt_savefile"), ex);
					}

					ProjectFile = fileName;
				}
			}
			finally {
				SilentWork = false;
			}
		}

		/// <exception cref="InvalidDataException">
		/// The save format is corrupt and could not load.
		/// </exception>
		/// <exception cref="ArgumentNullException">
		/// <paramref name="root"/> is null.
		/// </exception>
		protected virtual void Deserialize(XmlElement root)
		{
			if (root == null)
				throw new ArgumentNullException("root");

			XmlElement child = root["Language"];

			try {
				Language newLanguage = GetLanguage(child.InnerText);
				if (newLanguage == null)
					throw new InvalidDataException("Invalid project language.");

				Language = newLanguage;
			}
			catch (Exception ex) {
				throw new InvalidDataException("Invalid project language.", ex);
			}

			LoadEntitites(root);
			LoadRelations(root);

			//OnDeserializing(new SerializeEventArgs(root));
		}

		private Language GetLanguage(string languageName)
		{
			string languageString = languageName;
			Assembly assembly = Assembly.Load("NClass." + languageString);
			foreach (Type type in assembly.GetTypes()) {
				if (type.IsSubclassOf(typeof(Language))) {
					object languageInstance = type.InvokeMember("Instance",
						BindingFlags.Public | BindingFlags.Static | BindingFlags.GetProperty,
						null, null, null);
					return (languageInstance as Language);
				}
			}
			return null;
		}
		/// <exception cref="InvalidDataException">
		/// The save format is corrupt and could not load.
		/// </exception>
		/// <exception cref="ArgumentNullException">
		/// <paramref name="root"/> is null.
		/// </exception>
		private void LoadEntitites(XmlNode root)
		{
			if (root == null)
				throw new ArgumentNullException("root");

			XmlNodeList nodeList = root.SelectNodes("Entities/Entity");

			foreach (XmlElement node in nodeList) {
				try {
					string type = node.GetAttribute("type");

					IEntity entity = GetEntity(type);
					if (entity == null)
						throw new InvalidDataException("Invalid entity type: " + type);

					AddEntity(entity);
					entity.Deserialize(node);
				}
				catch (BadSyntaxException ex) {
					throw new InvalidDataException("Invalid entity.", ex);
				}
			}
		}

		private IEntity GetEntity(string type)
		{
			switch (type) {
				case "Class":
				case "CSharpClass":     // Old file format
				case "JavaClass":       // Old file format
					return Language.CreateClass();

				case "Structure":
				case "StructType":      // Old file format
					return Language.CreateStruct();

				case "Interface":
				case "CSharpInterface": // Old file format
				case "JavaInterface":   // Old file format
					return Language.CreateInterface();

				case "Enum":
				case "CSharpEnum":      // Old file format
				case "JavaEnum":        // Old file format
					return Language.CreateEnum();

				case "Delegate":
				case "DelegateType":    // Old file format
					return Language.CreateDelegate();

				case "Comment":
					return new Comment();

				default:
					return null;
			}
		}

		/// <exception cref="InvalidDataException">
		/// The save format is corrupt and could not load.
		/// </exception>
		/// <exception cref="ArgumentNullException">
		/// <paramref name="root"/> is null.
		/// </exception>
		private void LoadRelations(XmlNode root)
		{
			if (root == null)
				throw new ArgumentNullException("root");

			XmlNodeList nodeList = root.SelectNodes("Relations/Relation");

			foreach (XmlElement node in nodeList) {
				string type = node.GetAttribute("type");
				string firstString = node.GetAttribute("first");
				string secondString = node.GetAttribute("second");
				int firstIndex, secondIndex;

				if (!int.TryParse(firstString, out firstIndex) ||
					!int.TryParse(secondString, out secondIndex)) {
					throw new InvalidDataException(
						Strings.GetString("error_corrupt_save_format"));
				}
				if (firstIndex < 0 || firstIndex >= entities.Count ||
					secondIndex < 0 || secondIndex >= entities.Count)
				{
					throw new InvalidDataException(
						Strings.GetString("error_corrupt_save_format"));
				}

				try {
					IEntity first = entities[firstIndex];
					IEntity second = entities[secondIndex];
					Relation relation;

					switch (type) {
						case "Association":
							relation = AddAssociationRelation(first as TypeBase, second as TypeBase);
							break;

						case "Generalization":
							relation = AddGeneralizationRelation(
								first as CompositeType, second as CompositeType);
							break;

						case "Realization":
							relation = AddRealizationRelation(first as TypeBase, second as InterfaceType);
							break;

						case "Dependency":
							relation = AddDependencyRelation(first as TypeBase, second as TypeBase);
							break;

						case "Nesting":
							relation = AddNestingRelation(first as CompositeType, second as TypeBase);
							break;

						case "Comment":
						case "CommentRelation": // Old file format
							if (first is Comment)
								relation = AddCommentRelation(first as Comment, second);
							else
								relation = AddCommentRelation(second as Comment, first);
							break;

						default:
							throw new InvalidDataException(
								Strings.GetString("error_corrupt_save_format"));
					}
					relation.Deserialize(node);
				}
				catch (ArgumentNullException ex) {
					throw new InvalidDataException("Invalid relation.", ex);
				}
				catch (RelationException ex) {
					throw new InvalidDataException("Invalid relation.", ex);
				}
			}
		}

		/// <exception cref="IOException">
		/// Could not save the project.
		/// </exception>
		/// <exception cref="InvalidOperationException">
		/// The project was not saved before by the <see cref="Save(string)"/> method.
		/// </exception>
		public virtual bool Save()
		{
			if (ProjectFile == null)
				throw new InvalidOperationException(Strings.GetString("error_cannot_save_file"));
			
			Save(ProjectFile);

			return true;
		}

        public void LoadBinary(BinaryFormatter bformatter, Stream stream)
        {
            ClearProject();
            LoadEntititesBinary(bformatter,stream);
            LoadRelationsBinary(bformatter, stream);
            OnDeserializing(null);
        }
        public void SaveBinary(BinaryFormatter bformatter, Stream stream)
        {
            SaveEntititesBinary(bformatter, stream);
            SaveRelationsBinary(bformatter, stream);
            OnSerializing(null);
        }
		/// <exception cref="IOException">
		/// Could not save the project.
		/// </exception>
		/// <exception cref="ArgumentException">
		/// <paramref name="fileName"/> is empty string.
		/// </exception>
		public virtual void Save(string fileName)
		{
			if (string.IsNullOrEmpty(fileName))
				throw new ArgumentException(Strings.GetString("error_blank_filename"), "fileName");

			XmlDocument document = new XmlDocument();
			XmlElement root = document.CreateElement("ClassProject");
			document.AppendChild(root);

			Serialize(root);

			try {
				document.Save(fileName);
			}
			catch (Exception ex) {
				throw new IOException(Strings.GetString("error_could_not_save_file"), ex);
			}

			ProjectFile = fileName;
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="root"/> is null.
		/// </exception>
		protected virtual void Serialize(XmlElement root)
		{
			if (root == null)
				throw new ArgumentNullException("root");

			XmlElement child = root.OwnerDocument.CreateElement("Language");
			child.InnerText = Language.AssemblyName;
			root.AppendChild(child);

			SaveEntitites(root);
			SaveRelations(root);

			//OnSerializing(new SerializeEventArgs(root));
		}


        private void LoadEntititesBinary(BinaryFormatter bformatter, Stream stream)
        {
            int count = (int) bformatter.Deserialize(stream);
            entities.Clear();
            for (int i = 0; i < count; i++ )
            {
                IEntity ie = (IEntity)bformatter.Deserialize(stream);
                AddEntity(ie);
                if (ie is TypeBase)
                {
                    (ie as TypeBase).OnDeserializing(new SerializeEventArgsBinary(
                        bformatter, stream));
                    if (ie is ClassType)
                    {
                        (ie as ClassType).raptorTab = raptorUpdater.createClass(ie.Name, ie as ClassType);
                        foreach (Operation operation in (ie as ClassType).Operations)
                        {
                            if (operation is Method && !operation.IsAbstract)
                            {
                                (operation as Method).raptorTab = ProjectCore.raptorUpdater.createMethod(
                                    (ie as ClassType).raptorTab,
                                    operation.Name,
                                    operation as Method);
                            }
                        }
                        ProjectCore.raptorUpdater.resetAttributes(
                                    (ie as ClassType).raptorTab,
                                    (ie as ClassType).Fields);
                    }
                }
                else if (ie is Comment)
                {
                    (ie as Comment).OnDeserializing(new SerializeEventArgsBinary(
                        bformatter, stream));
                }
            }
        }
        public ClassType findClass(string name)
        {
            foreach (IEntity entity in entities)
            {
                if ((entity is ClassType) && (entity.Name.ToLower().CompareTo(name.ToLower()) == 0))
                    return entity as ClassType;
            }
            return null;
        }
        private void SaveEntititesBinary(BinaryFormatter bformatter, Stream stream)
        {
            bformatter.Serialize(stream, entities.Count);
            foreach (IEntity entity in entities)
            {
                bformatter.Serialize(stream, entity);
                if (entity is TypeBase)
                {
                    (entity as TypeBase).OnSerializing(new SerializeEventArgsBinary(
                        bformatter, stream));
                }
                else if (entity is Comment)
                {
                    (entity as Comment).OnSerializing(new SerializeEventArgsBinary(
                        bformatter, stream));
                }
            }
        }

		/// <exception cref="ArgumentNullException">
		/// <paramref name="root"/> is null.
		/// </exception>
		private void SaveEntitites(XmlElement root)
		{
			if (root == null)
				throw new ArgumentNullException("root");

			XmlElement entitiesChild = root.OwnerDocument.CreateElement("Entities");

			foreach (IEntity entity in entities) {
				XmlElement child = root.OwnerDocument.CreateElement("Entity");

				entity.Serialize(child);
				child.SetAttribute("type", entity.EntityType);
				entitiesChild.AppendChild(child);
			}
			root.AppendChild(entitiesChild);
		}

        private void LoadRelationsBinary(BinaryFormatter bformatter, Stream stream)
        {
            int count = (int)bformatter.Deserialize(stream);
            relations.Clear();
            for (int i = 0; i < count; i++)
            {
                int firstIndex = (int)bformatter.Deserialize(stream);
                int secondIndex = (int)bformatter.Deserialize(stream);
                BinarySerializationHelper.first_entity = entities[firstIndex];
                BinarySerializationHelper.second_entity = entities[secondIndex];
                Relation rel = (Relation)bformatter.Deserialize(stream);
                AddRelation(rel);
                rel.OnDeserializing(new SerializeEventArgsBinary(bformatter, stream));
            }
        }
        private void SaveRelationsBinary(BinaryFormatter bformatter, Stream stream)
        {
            bformatter.Serialize(stream, relations.Count);
			foreach (Relation relation in relations) {
                int firstIndex = entities.IndexOf(relation.First);
                int secondIndex = entities.IndexOf(relation.Second);
                bformatter.Serialize(stream, firstIndex);
                bformatter.Serialize(stream, secondIndex);
                bformatter.Serialize(stream, relation);
                relation.OnSerializing(new SerializeEventArgsBinary(bformatter, stream));
            }
        }
		/// <exception cref="ArgumentNullException">
		/// <paramref name="root"/> is null.
		/// </exception>
		private void SaveRelations(XmlNode root)
		{
			if (root == null)
				throw new ArgumentNullException("root");

			XmlElement relationsChild = root.OwnerDocument.CreateElement("Relations");
			
			foreach (Relation relation in relations) {
				XmlElement child = root.OwnerDocument.CreateElement("Relation");

				int firstIndex  = entities.IndexOf(relation.First);
				int secondIndex = entities.IndexOf(relation.Second);

				relation.Serialize(child);
				child.SetAttribute("type", relation.Name);
				child.SetAttribute("first", firstIndex.ToString());
				child.SetAttribute("second", secondIndex.ToString());
				relationsChild.AppendChild(child);
			}
			root.AppendChild(relationsChild);
		}

		protected virtual void OnCleared(EventArgs e)
		{
			if (Cleared != null)
				Cleared(this, e);
            OnFileStateChanged(EventArgs.Empty);
		}

		protected virtual void OnContentChanged(EventArgs e)
		{
			contentModified = false;
			if (ContentChanged != null)
				ContentChanged(this, e);
		}

		protected virtual void OnLanguageChanged(EventArgs e)
		{
			if (LanguageChanged != null)
				LanguageChanged(this, e);
		}

		protected virtual void OnFileStateChanged(EventArgs e)
		{
			if (FileStateChanged != null)
				FileStateChanged(this, e);
		}

		protected virtual void OnEntityAdded(EntityEventArgs e)
		{
			if (EntityAdded != null)
				EntityAdded(this, e);
			ContentModified();
		}

		protected virtual void OnEntityRemoved(EntityEventArgs e)
		{
			if (EntityRemoved != null)
				EntityRemoved(this, e);
			ContentModified();
		}

		protected virtual void OnRelationAdded(RelationEventArgs e)
		{
			if (RelationAdded != null)
				RelationAdded(this, e);
			ContentModified();
		}

		protected virtual void OnRelationRemoved(RelationEventArgs e)
		{
			if (RelationRemoved != null)
				RelationRemoved(this, e);
			ContentModified();
		}

		protected virtual void OnSerializing(SerializeEventArgsBinary e)
		{
			if (Serializing != null)
				Serializing(this, e);
		}

		protected virtual void OnDeserializing(SerializeEventArgsBinary e)
		{
			if (Deserializing != null)
				Deserializing(this, e);
			ContentModified();
		}
	}
}
