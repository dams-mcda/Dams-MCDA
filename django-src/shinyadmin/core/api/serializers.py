from ..models import DamsMCDARunPreference, DamsMCDAGroup
from rest_framework import serializers

class RunPreferenceSerializer(serializers.ModelSerializer):
    group = serializers.PrimaryKeyRelatedField(many=False, allow_null=True, queryset=DamsMCDAGroup.objects.all())
    class Meta:
        model = DamsMCDARunPreference
        fields = '__all__'
