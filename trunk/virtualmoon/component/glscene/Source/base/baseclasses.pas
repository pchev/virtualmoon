//
// This unit is part of the GLScene Project, http://glscene.org
//
{: Base classes for GLScene.<p>

   <b>History : </b><font size=-1><ul>
      <li>05/10/08 - DanB - Creation, from GLMisc.pas + other places
   </ul></font>
}

unit BaseClasses;

interface

uses Classes, PersistentClasses, GLCrossPlatform;

type

   // TProgressTimes
   //
   TProgressTimes = record
      deltaTime, newTime : Double
   end;

   // TGLProgressEvent
   //
   {: Progression event for time-base animations/simulations.<p>
      deltaTime is the time delta since last progress and newTime is the new
      time after the progress event is completed. }
   TGLProgressEvent = procedure (Sender : TObject; const deltaTime, newTime : Double) of object;


   IGLNotifyAble = interface(IInterface)
   ['{00079A6C-D46E-4126-86EE-F9E2951B4593}']
     procedure NotifyChange(Sender : TObject);
   end;

  IGLProgessAble = interface(IInterface)
  ['{95E44548-B0FE-4607-98D0-CA51169AF8B5}']
			procedure DoProgress(const progressTime : TProgressTimes);
  end;

   // TGLUpdateAbleObject
   //
   {: An abstract class describing the "update" interface.<p> }
   TGLUpdateAbleObject = class (TGLInterfacedPersistent, IGLNotifyAble)
      private
	      { Private Declarations }
         FOwner : TPersistent;
         FUpdating : Integer;
         FOnNotifyChange : TNotifyEvent;

      public
	      { Public Declarations }
         constructor Create(AOwner: TPersistent); virtual;

			procedure NotifyChange(Sender : TObject); virtual;
         function GetOwner : TPersistent; override;

         property Updating : Integer read FUpdating;
         procedure BeginUpdate;
         procedure EndUpdate;

         property Owner : TPersistent read FOwner;
         property OnNotifyChange : TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
	end;

	// TGLCadenceAbleComponent
	//
	{: A base class describing the "cadenceing" interface.<p> }
	TGLCadenceAbleComponent = class (TGLComponent, IGLProgessAble)
		public
	      { Public Declarations }
			procedure DoProgress(const progressTime : TProgressTimes); virtual;
	end;

	// TGLUpdateAbleComponent
	//
	{: A base class describing the "update" interface.<p> }
	TGLUpdateAbleComponent = class (TGLCadenceAbleComponent, IGLNotifyAble)
		public
	      { Public Declarations }
			procedure NotifyChange(Sender : TObject); virtual;
	end;

   // TNotifyCollection
   //
   TNotifyCollection = class (TOwnedCollection)
      private
	      { Private Declarations }
         FOnNotifyChange : TNotifyEvent;

      protected
	      { Protected Declarations }
         procedure Update(item : TCollectionItem); override;

      public
	      { Public Declarations }
         constructor Create(AOwner : TPersistent; AItemClass : TCollectionItemClass);
         property OnNotifyChange : TNotifyEvent read FOnNotifyChange write FOnNotifyChange;
   end;


implementation

//---------------------- TGLUpdateAbleObject -----------------------------------------

// Create
//
constructor TGLUpdateAbleObject.Create(AOwner: TPersistent);
begin
	inherited Create;
	FOwner:=AOwner;
end;

// NotifyChange
//
procedure TGLUpdateAbleObject.NotifyChange(Sender : TObject);
begin
   if (FUpdating=0) and Assigned(Owner) then begin
      if Owner is TGLUpdateAbleObject then
         TGLUpdateAbleObject(Owner).NotifyChange(Self)
      else if Owner is TGLUpdateAbleComponent then
         TGLUpdateAbleComponent(Owner).NotifyChange(Self);
      if Assigned(FOnNotifyChange) then
         FOnNotifyChange(Self);
   end;
end;

// GetOwner
//
function TGLUpdateAbleObject.GetOwner : TPersistent;
begin
   Result:=Owner;
end;

// BeginUpdate
//
procedure TGLUpdateAbleObject.BeginUpdate;
begin
   Inc(FUpdating);
end;

// EndUpdate
//
procedure TGLUpdateAbleObject.EndUpdate;
begin
   Dec(FUpdating);
   if FUpdating<=0 then begin
      Assert(FUpdating=0);
      NotifyChange(Self);
   end;
end;

// ------------------
// ------------------ TGLCadenceAbleComponent ------------------
// ------------------

// DoProgress
//
procedure TGLCadenceAbleComponent.DoProgress(const progressTime : TProgressTimes);
begin
   // nothing
end;

// ------------------
// ------------------ TGLUpdateAbleObject ------------------
// ------------------

// NotifyChange
//
procedure TGLUpdateAbleComponent.NotifyChange(Sender : TObject);
begin
   if Assigned(Owner) then
   if (Owner is TGLUpdateAbleComponent) then
      (Owner as TGLUpdateAbleComponent).NotifyChange(Self);
end;

// ------------------
// ------------------ TNotifyCollection ------------------
// ------------------

// Create
//
constructor TNotifyCollection.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass);
begin
   inherited Create(AOwner,AItemClass);
   if Assigned(AOwner) and (AOwner is TGLUpdateAbleComponent) then
      OnNotifyChange:=TGLUpdateAbleComponent(AOwner).NotifyChange;
end;

// Update
//
procedure TNotifyCollection.Update(Item: TCollectionItem);
begin
   inherited;
   if Assigned(FOnNotifyChange) then
      FOnNotifyChange(Self);
end;

end.
